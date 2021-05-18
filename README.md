# dotfiles
My linux configuration

for emacs do this after cloning the repo
cd ~
rm -rf .emacs.d
ln -s emacs.d .emacs.d



# tmux configuration
https://github.com/gpakosz/.tmux

# zsh
## theme
https://github.com/romkatv/powerlevel10k
brew install romkatv/powerlevel10k/powerlevel10k
echo "source $(brew --prefix)/opt/powerlevel10k/powerlevel10k.zsh-theme" >>~/.zshrc

## modules
zsh-autosuggestions
