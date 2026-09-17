//! Tylax adapter. Expand simple math definitions before source conversion:
//! tylax 0.3.8 does not resolve bare macro calls inside math on its own.
use serde_json::{Value, json};
use std::{collections::HashMap, path::Path};
use typst::syntax::{SyntaxKind as K, SyntaxNode, ast, ast::AstNode, parse, parse_math};

type Result<T> = std::result::Result<T, String>;

pub fn convert(preamble: &str, source: &str, root: &Path, display: bool) -> Result<Value> {
    let mut converter = Converter {
        definitions: HashMap::new(),
        substitutions: Vec::new(),
        display,
    };
    converter.definitions(preamble, root)?;
    let body = converter.math(&parse_math(source), &HashMap::new())?;
    Ok(json!({"latex": body, "packages": ["amsmath", "amssymb"], "backend": "tylax"}))
}

struct Converter {
    definitions: HashMap<String, SyntaxNode>,
    substitutions: Vec<(String, String)>,
    display: bool,
}

impl Converter {
    fn definitions(&mut self, source: &str, root: &Path) -> Result<()> {
        for node in parse(source).children() {
            if let Some(binding) = node.cast::<ast::LetBinding>() {
                if let (Some(name), Some(value)) =
                    (binding.kind().bindings().first(), binding.init())
                {
                    self.definitions
                        .insert(name.get().to_string(), value.to_untyped().clone());
                }
            } else if let Some(import) = node.cast::<ast::ModuleImport>()
                && let (ast::Expr::Str(path), Some(ast::Imports::Wildcard)) =
                    (import.source(), import.imports())
            {
                let path = root.join(path.get().as_str());
                let text = std::fs::read_to_string(&path).map_err(|e| e.to_string())?;
                self.definitions(&text, path.parent().unwrap_or(root))?;
            }
        }
        Ok(())
    }

    // Opaque slots carry already-converted arguments through tylax. Braces keep
    // an argument grouped when a macro uses it as a numerator, exponent, etc.
    fn slot(&mut self, latex: String) -> String {
        let name = format!("orgtypstslot{}end", self.substitutions.len());
        self.substitutions
            .push((name.clone(), format!("{{{latex}}}")));
        name
    }

    fn math(&mut self, node: &SyntaxNode, locals: &HashMap<String, String>) -> Result<String> {
        let source = self.render(node, locals)?;
        let options = tylax::T2LOptions {
            math_only: true,
            block_math_mode: self.display,
            ..Default::default()
        };
        let mut latex = tylax::typst_to_latex_with_options(&source, &options);
        for (name, value) in self.substitutions.iter().rev() {
            latex = latex.replace(name, value);
        }
        Ok(latex)
    }

    fn value(&mut self, node: &SyntaxNode, locals: &HashMap<String, String>) -> Result<String> {
        // Keep this a math-macro adapter, not another Typst interpreter.
        match node.kind() {
            K::Equation => self.math(
                node.cast::<ast::Equation>().unwrap().body().to_untyped(),
                locals,
            ),
            _ => self.math(node, locals),
        }
    }

    fn call(
        &mut self,
        definition: &SyntaxNode,
        args: Vec<ast::Arg<'_>>,
        locals: &HashMap<String, String>,
    ) -> Result<String> {
        let Some(closure) = definition.cast::<ast::Closure>() else {
            let name = definition.full_text();
            if let Some(alias) = self.definitions.get(name.as_str()).cloned() {
                return self.call(&alias, args, locals);
            }
            let name = self.render(definition, locals)?;
            let args = args
                .into_iter()
                .map(|arg| self.render(arg.to_untyped(), locals))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            return Ok(format!("{name}({args})"));
        };
        let mut bindings = HashMap::new();
        for (param, arg) in closure.params().children().zip(args) {
            let name = param.to_untyped().full_text().to_string();
            bindings.insert(name, self.math(arg.to_untyped(), locals)?);
        }
        let latex = self.value(closure.body().to_untyped(), &bindings)?;
        Ok(self.slot(latex))
    }

    fn render(&mut self, node: &SyntaxNode, locals: &HashMap<String, String>) -> Result<String> {
        if matches!(node.kind(), K::Ident | K::MathIdent) {
            let name = node.full_text();
            if let Some(value) = locals.get(name.as_str()) {
                return Ok(self.slot(value.clone()));
            }
            if let Some(value) = self.definitions.get(name.as_str()).cloned() {
                let latex = self.value(&value, &HashMap::new())?;
                return Ok(self.slot(latex));
            }
        }
        if let Some(call) = node.cast::<ast::MathCall>() {
            let name = call.callee().to_untyped().full_text();
            if let Some(definition) = self.definitions.get(name.as_str()).cloned() {
                return self.call(
                    &definition,
                    call.args().arg_items().map(|a| a.arg).collect(),
                    locals,
                );
            }
            // Typst mat defaults to parentheses; tylax otherwise emits matrix.
            if name == "mat"
                && !call
                    .args()
                    .arg_items()
                    .any(|a| matches!(a.arg, ast::Arg::Named(n) if n.name().get() == "delim"))
            {
                let args = self.render(call.args().to_untyped(), locals)?;
                return Ok(format!("mat(delim: \"(\", {})", &args[1..args.len() - 1]));
            }
        }
        if let Some(call) = node.cast::<ast::FuncCall>() {
            let name = call.callee().to_untyped().full_text();
            if let Some(definition) = self.definitions.get(name.as_str()).cloned() {
                return self.call(&definition, call.args().items().collect(), locals);
            }
        }
        if matches!(node.kind(), K::FieldAccess | K::MathFieldAccess) {
            let name = node.full_text();
            if let Some(name) = name
                .strip_prefix("math.")
                .or_else(|| name.strip_prefix("sym."))
            {
                return Ok(name.to_owned());
            }
        }
        if node.kind() == K::Equation {
            let latex = self.value(node, locals)?;
            return Ok(self.slot(latex));
        }
        if node.kind() == K::Hash {
            return Ok(String::new());
        }
        if node.children().next().is_none() {
            return Ok(node.full_text().to_string());
        }
        node.children()
            .map(|child| self.render(child, locals))
            .collect()
    }
}
