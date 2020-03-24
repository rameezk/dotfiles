k-delete-pod-with-status() {
    if [[ -z "$1" ]]; then
        echo "Please specify a status"
        return 1
    fi

    k get pods | grep "$1" | awk '{print $1}' | xargs kubectl delete pod
}

k-patch-img-pull-secret() {
    if [[ -z "$1" ]]; then
        echo "Please specify a secret name"
        return 1
    fi

    k patch serviceaccount default -p "{\"imagePullSecrets\": [{\"name\": \"$1\"}]}"
}
