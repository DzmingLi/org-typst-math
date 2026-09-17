"""Build independently installable package.el archives in work/packages/."""
from io import BytesIO
from pathlib import Path
import tarfile

root = Path(__file__).resolve().parent.parent
out = root / "work/packages"
out.mkdir(parents=True, exist_ok=True)
version = "0.1.0"
packages = {
    "org-fragtog-plus": (
        ["org-fragtog-plus.el"],
        '((emacs "30.2") (org "9.7.11"))',
        "Automatic Org formula previews",
    ),
    "typst-client": (
        ["typst-client.el"],
        '((emacs "30.2"))',
        "Persistent Typst conversion service",
    ),
    "org-typst-math": (
        ["org-typst-math.el", "org-typst-math-export.el", "org-typst-math-preview.el"],
        '((emacs "30.2") (org "9.7.11") (typst-client "0.1.0"))',
        "Typst mathematics in Org",
    ),
}
for name, (files, dependencies, description) in packages.items():
    prefix = f"{name}-{version}"
    contents = {file: (root / "lisp" / file).read_bytes() for file in files}
    contents[f"{name}-pkg.el"] = (
        ";;; -*- no-byte-compile: t; lexical-binding: t; -*-\n"
        f'(define-package "{name}" "{version}" "{description}" \'{dependencies})\n'
    ).encode()
    path = out / f"{prefix}.tar"
    with tarfile.open(path, "w", format=tarfile.GNU_FORMAT) as archive:
        for file, data in contents.items():
            info = tarfile.TarInfo(f"{prefix}/{file}")
            info.size = len(data)
            info.mode = 0o644
            archive.addfile(info, BytesIO(data))
    print(path.relative_to(root))
