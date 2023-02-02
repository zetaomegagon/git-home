# git-home
Tracking various $HOME dir configurations with a git bare repo

## Branches
- [main](https://github.com/zetaomegagon/git-home/tree/main)  - aimed to be a generic config branch for any nix disto (biased toward Fedora and Gnome rn)
- [panda](https://github.com/zetaomegagon/git-home/tree/panda) - my current laptop config. Most interesting stuff is in here

## Context
- Below code is [found here](https://github.com/zetaomegagon/git-home/blob/main/.bashrc.d/01-functions.rc#L5-L32)
- recommended that a git status prompt is used. Git comes with one disabled by the way.
- don't turn off untracked file listing for `git status`; rather, use a `.gitignore` file as an include list
- or: `git add --force <whatever>`
- using [git-crypt](https://github.com/AGWA/git-crypt) from Fedora repos. Both `.gnupg/**` and `.ssh/**` are encrypted

## Git Wrappers (Syntax Sugar)
wrapper pointing git to bare `work-tree` and `git-dir`
```
export GIT_HOME_DIR=".git-home-$HOSTNAME"

git-home() {
    command git --git-dir="$GIT_HOME_DIR" --work-tree="$HOME" "$@"
}
```

initializes --bare repo, additionally adds a `.gitignore` that excludes everything by default. Selectively `--force` add or create exceptions in `.gitignore`
```
git-home-init() {
    echo '*' > "$HOME"/.gitignore
    command git init --bare "$HOME"/"$GIT_HOME_DIR"
}
```


this git wrapper makes git just work like git. Most solutions I've found forget use a clunky function or alias. This just uses a simple conditional to check if in the work-dir. Additionally, no need to add a case for `git-crypt`. It actually gets passed into `git-home`. So you can call it like `git crypt ...`
```
git() {
    local args=( "$@" )
    local first="${args[0]}"
    local rest=( "${args[@]:1:${#args}}" )

    if [[ $PWD = $HOME ]]; then
	case "$first" in
	    init)
		git-home-init ;;
	    *)
		git-home "$first" "${rest[@]}"
	esac	
    else
	command git "${args[@]}"
    fi
}
```
