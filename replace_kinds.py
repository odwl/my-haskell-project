import os
import re

files = [
    "docs/annex_n_ary_glues.md",
    "docs/outline.md",
    "docs/03_the_aggregators.md",
    "docs/01_the_atoms.md",
]

def replace_kinds(text):
    # Replace * -> * -> *
    text = text.replace("`* -> * -> *`", "`Type -> Type -> Type`")
    # Replace * -> *
    text = text.replace("`* -> *`", "`Type -> Type`")
    # Replace [* -> *]
    text = text.replace("[* -> *]", "[Type -> Type]")
    
    # Replace kind `*`
    text = text.replace("kind `*`", "kind `Type`")
    text = text.replace("kinds (`*`)", "kinds (`Type`)")
    text = text.replace("types (`*`)", "types (`Type`)")
    text = text.replace("Monoids (`*`)", "Monoids (`Type`)")
    text = text.replace("(`*`)", "(`Type`)") # Check this one manually 

    # Replace specific headers
    text = text.replace("(`*`)", "(`Type`)")
    
    return text

for f in files:
    with open(f, "r") as file:
        content = file.read()
    
    # Let's fix the multiplication operator back if it was replaced
    new_content = replace_kinds(content)
    new_content = new_content.replace("`1` and `Type`", "`1` and `*`") # Fix multiplication
    
    with open(f, "w") as file:
        file.write(new_content)

print("Done")
