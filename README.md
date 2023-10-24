# git-home
Tracking various personal machine configurations with my $HOME directory as a git bare repo

## Impetus
I currently don't have a way to (mostly) effortlessly backup various machine configuration, and as a result, I've lost various config files in a short period of time. It's set me back, timewise, and recrafting these files from memory isn't currently possible while I have an 11 m/o son climbing all over the place. This solution fits the specific case of backing up the mentioned files with the added benefit of version control.

## Branches
- [main](https://github.com/zetaomegagon/git-home/tree/main)  - aimed to be a generic config branch for any nix disto (currently biased toward Fedora and Gnome).
- [panda](https://github.com/zetaomegagon/git-home/tree/panda) - my current laptop config. Most changes are in this branch.

## Notes
- Below code is [found here](https://github.com/zetaomegagon/git-home/blob/panda/.bashrc.d/01-functions.rc#L5-L32).
- I recommended that a git status prompt is used. Git comes with one disabled. Checkout The Git Book. [[Bash](https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Bash)|[Zsh](https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Zsh)|[PowerShell](https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-PowerShell)]
- These are all the [environment variables](https://github.com/zetaomegagon/git-home/blob/panda/.bashrc.d/11-git-prompt-options.rc) I'm using with git's prompt. They are all documented in
	- `/usr/share/doc/git/contrib/completion/git-prompt.sh`
- Git also has various completions
	- `/usr/share/doc/git/contrib/completion/git-completion.bash`
	- `/usr/share/doc/git/contrib/completion/git-completion.tcsh`
	- `/usr/share/doc/git/contrib/completion/git-completion.zsh`
- I don't turn off untracked file listing for `git status`; rather, I use a `.gitignore` file as an include list so that I only see the status of untracked files I care about.
- or: `git add --force <whatever>`; then later add to `.gitignore`
- For sensetive files / directories, I'm using [git-crypt](https://github.com/AGWA/git-crypt) from the Fedora repos (but the package is supported in other distros too). Right now, I'm using `git-crypt` for `.gnupg/**` and `.ssh/**` directories.

## Git Wrappers for interacting with git / git-home
Wrapper pointing git to bare `work-tree` and `git-dir`
```
export GIT_HOME_DIR=".git-home-$USER-$HOSTNAME"

git-home() {
    # run git and specify the git directory as well as the working tree
    command git --git-dir="$GIT_HOME_DIR" --work-tree="$HOME" "$@"
}
```

This initializes `--bare` repo, additionally adds a `.gitignore` that excludes everything by default. Selectively `--force` add or create exceptions in `.gitignore`
```
git-home-init() {
    # populate a .gitignore file and excluding everything
    echo '*' > "$HOME"/.gitignore
    
    # initialize the bare repo in your home directory
    command git init --bare "$HOME"/"$GIT_HOME_DIR"
}
```


This git wrapper makes git just work like git. Most solutions I've found use a clunky function or alias, and this feels awkward to me. The function is using a simple conditional to check if in the `work-dir`, and to use `git-home` if so; otherwise call the git binary directly. Additionally, no need to add a case for `git-crypt`. It actually gets passed into `git-home` and acts like a git module (where you can call, for instance `git <module>` or `git-<module>`). So you can call it like `git crypt <args>`
```
git() {
    # read-in arguments to an array ARGS
    readarray args < <(printf -- "$*")
    
    # capture the git command, for instance 'push'
    local first="${args[0]}"
    
    # capture the rest of the arguments, for instancs '-u origin panda'
    readarray rest < <(printf -- "${args[@]:1:${#args[@]}}")

    # check if in the home dir bare repo.
    # If so, use 'git-home' or 'git-home-init'; otherwise, call the git binary directly
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
## Example
Add the above functions to some file that you source. I use a file in `$HOME/.bashrc.d/`, but you can use `.bashrc` or `.profile` depending on your setup, then ...
```
~$ cd
~$ source ~/.bashrc.d/01-functions.rc
~$ git-home-init
~$ ls -Al | grep -E "$GIT_HOME_DIR|.gitignore"
~$ sep(){ for i in {1..79}; do ((i < 79 )) && printf = || printf "\n"; done; }; sep; cat .gitignore; sep; git status; sep
~$ echo '!.gitignore' >> .gitignore
~$ git status
```

At this point you should see that `.gitignore` isn't tracked by git. If you want to keep this change (you most likely will), you can `git add -A`, or if paranoid, `git add .gitignore`; then `git commit -m ...` as usual. If you don't want to keep the change, you can use whatever you want to remove the change (editor, sed), or you can do `git restore .gitignore`.

Overall I think this setup is pretty handy.
