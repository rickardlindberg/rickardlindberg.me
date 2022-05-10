while true; do
    echo "$@"
    "$@" || echo "restarting"
done
