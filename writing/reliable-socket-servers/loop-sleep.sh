while true; do
    echo "$@"
    "$@" || true
    echo "restarting"
    sleep 60
done
