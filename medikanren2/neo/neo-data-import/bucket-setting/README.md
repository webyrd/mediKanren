The Bash script, **`run.sh`**, performs the following tasks in order:

1. Runs a Racket file (`rtx-kg2-publication-distribution.rkt`).
2. Checks if the Python package `pandas` is installed; if not, installs it.
3. Runs a Python file (`generate-publication-dist.py`).

Use this script to automate your workflow without having to manually execute each command.

---

## Prerequisites

- **Bash** (or a Bash-compatible shell)  
  - If you’re on macOS or a Linux distribution, Bash is usually installed by default.  
  - On Windows, you can use [Git Bash](https://git-scm.com/downloads) or the Windows Subsystem for Linux (WSL).
- **Racket**  
  - Make sure the `racket` command is available on your system path.
- **Python 3**  
  - Confirm you can run `python3` in your terminal.
- **pip3**  
  - If you can run `pip3 --version`, it’s installed.

---

## Getting Started

1. **Have the data ready** Please ensure that the data is stored in the directories specified in the Racket and Python files.

2. **Make the script executable** (if it isn’t already):
   ```bash
   chmod +x run.sh

3. **Run the script:**
  ```bash
  ./run.sh
