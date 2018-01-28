# Vim + Zsh + Tmux config

Created a dotfile folder for my vim configuratin.  
You need to have symbolic link of the .vimrc .zshrc .tmux.conf and .vim in you ~/ directory.  
You need to have a terminal with 24 bit color support as well as confighuring tmux for 24 bit color.  
You can run the vim-layout script (in the tmux folder) to run tmux with vim in the left (85%), and zsh in the right (15%)  
you have access to a lot of plugins that i use, linked in the .vim folder. install vundle for a full support of my .vimrc and do a :PluginInstall to install all plugins.

For a windows .vimrc install with git bash installed (and vim as the edition program (ie. recommanded editor for git bash)):  
git clone https://github.com/Narice/dotfiles/ ~  
mv ~/dotfiles/vim/.virmc ~  
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim  
rm -rf ~/dotfiles/  
vim  
(then, do a :PluginInstall)  
if you want to install a powerline font, I recommand the Inconsolata for powerline font  
to install, go to:  
https://github.com/powerline/fonts/blob/master/Inconsolata/Inconsolata%20for%20Powerline.otf  
click download, then install it by double clicking it.  
Then, go to your settings in the git bash terminal (mintty) and go to text, then click select, you should see the font.  

Have fun
