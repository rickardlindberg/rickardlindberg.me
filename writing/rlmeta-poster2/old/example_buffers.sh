compile() {
    python rlmeta.py --support
    cat example_buffers.py
    cat example_buffers.rlmeta | python rlmeta.py
    echo "print(ExampleBuffers().run(\"program\","
    cat
    echo "))"
}

python <(compile)
