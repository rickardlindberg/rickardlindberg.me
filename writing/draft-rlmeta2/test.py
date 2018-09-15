import support

print(support._ListBuilder([
    "class Hello(object):\n",
    support._IndentBuilder(),
    support._ListBuilder([
        "pass\n\n",
    ]),
    support._DedentBuilder(),
    "the end\n"
]).to_rlmeta_output_stream())
