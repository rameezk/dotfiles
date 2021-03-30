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

k-exec-into() {
    # Interactively exec a shell in a kubernetes pod
    pod=$(kubectl get pods --no-headers | awk '{print $1}' | fzf)
    shell=$(echo "bash\nsh\nzsh\nfish" | fzf)

    kubectl exec -it $pod -- $shell
}


