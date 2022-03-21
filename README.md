# dotfiles
dotfiles

applications list:
vim
doom-emacs
hammerspoon
mackup
tmux

## 使用方法
mackup的配置文件见`dotfiles/home/.mackup.cfg`.
1. MacOS使用`brew install mackup`, Linux使用`pip install --upgrade mackup`下载安装mackup软
   件。
2. 拷贝`dotfiles/home/.mackup.cfg`文件到`~`目录下，执行`mackup restore`.

``` sh
ln -s -f dotfiles/home/.mackup.cfg

mackup restore
```

## tmux 

在 `$HOME` 目录下执行，将该文件链接到当前目下
```sh
ln -s -f .tmux/.tmux.conf
```
