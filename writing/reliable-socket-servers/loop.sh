while true; do
    echo "$@"
    "$@" || true
    echo "restarting"
done
