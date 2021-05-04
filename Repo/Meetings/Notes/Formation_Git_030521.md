## Meeting One  - Formation Git

### Commits

Commits are the unit of work of git
- They identify a version of your project
- They may or may not have predecessors
- They may or may not have successors
- It's possible to compute a diff for a given commit
- They are uniquely identified by a hash

### Index vs. Worktree

**Worktree** 

### Branches

Branches are cursors pointing to one commit each (called its HEAD)
- A git repository can have as many branches as the developer sees fit
- A git repository has one current banch

### Rebase and Merges

- What to do when branches diverge ?

Merge : Create a commit to reconcile two (or more) branches
Rebase : Creqte copy of (replay) one branch's commits on tope of the second brance.

In both cases, confilictes are like to occur.

### Git worktrees

- A git repository can support multiple working trees, allowing you to check out more than one branch at a time
- A repository has one


### Git configuration

- git config (--global) user.name "XX YY"

### Magit



### Practice

**Part 1**

- add .gitignore file firstly

- git diff
    - git diff staged
    - 

- git add -p : to choose from add the modified file

- git reset : remove the changes

- git restore -p: reverse

**Part 2**

- git branch
- git switch -c typo
- git switch -C typo
