import os

def main():
    # Path to src folder (sibling of this Python file)
    base_dir = os.path.dirname(os.path.abspath(__file__))
    src_dir = os.path.join(base_dir, "src")
    output_file = os.path.join(base_dir, "code.txt")

    # Collect all .rs files under src (recursively)
    rs_files = []
    for root, _, files in os.walk(src_dir):
        for f in files:
            if f.endswith(".rs"):
                if f not in ["io.rs", "perft.rs", "gui_piston.rs", "mod.rs"]:
                    print(f"Added file: {f}")
                    rs_files.append(os.path.join(root, f))

    # Sort files for deterministic order
    rs_files.sort()

    # Concatenate content
    with open(output_file, "w", encoding="utf-8") as out:
        for filepath in rs_files:
            with open(filepath, "r", encoding="utf-8") as f:
                out.write(f.read())
                out.write("\n\n")  # Add spacing between files

    print(f"Concatenated {len(rs_files)} .rs files into {output_file}")

if __name__ == "__main__":
    main()
