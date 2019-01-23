#!/bin/sh

kubectl() {
    source <(command kubectl completion zsh)
    command kubectl "$@"
}

alias k='kubectl'
alias kc='kubectx'
alias kn='kubens'
alias kf='sudo kubefwd services -n '

#Current cluster
alias kcc='k config current-context'

alias sk='kubectl -n kube-system'
alias ke='EDITOR=$EDITOR kubectl edit'
alias klbaddr="kubectl get svc -ojsonpath='{.status.loadBalancer.ingress[0].hostname}'"

# alias kdebug='kubectl run -i -t debug --rm --image=caarlos0/debug --restart=Never'
alias knrunning='kubectl get pods --field-selector=status.phase!=Running'
alias kfails='kubectl get po -owide --all-namespaces | grep "0/" | tee /dev/tty | wc -l'
alias kimg="kubectl get deployment --output=jsonpath='{.spec.template.spec.containers[*].image}'"

alias kdebug='kubectl run --rm -i -t debug --image=rameezk/debuggery --restart=Never'
