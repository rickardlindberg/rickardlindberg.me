import support

print(support._Builder.create([
    "class Hello(object):\n",
    support._IndentBuilder(),
    [
        "pass\n\n",
    ],
    support._DedentBuilder(),
    "the end\n"
]).to_rlmeta_output_stream())
