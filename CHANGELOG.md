# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

### Added

- Icon for LUA (from LUA)
- Icon for emmet minor mode.
- Icon for spacemacs buffer.
- Icon for Isearch (from Font Awesome)
- Icon for Org-Agenda (From Font Awesome)
- Icon for Powershell
- Icon for Rust
- Icon for Windows cmd
- Icon for MSYS shell
- Icon for Cygwin shell
- Icon for ivy-mode
- Icon for Elixir
- Icon for Erlang
- Added option mode-icons-line-height-adjust to allow imagmagick to
  adjust icons up or down a bit compared to what emacs returns as the
  mode-line height.

### Changed

- Git Flavored markdown also uses markdown icon from Font Awesome.
- Minor modes icons are now updated:
  - After changing a major mode
  - After activating isearch-mode (new)
  - After executing a command with `command-execute` (new). This
    allows `execute-extended-command` and `ido` to display the icons
    immediately after activating an iconified mode.
- Powerline now recolors lines correctly by modified powerline-raw
  function.
- mode-icons now recolors font icons correctly when the font is
  missing from the system
- mode-icons now allows for a default icon.  This can be turned on
  with the new option `mode-icons-use-default-icon`
- Fix coloring algorithm for xpm images.
- Fix icon for Shell-script.
- Response to emojify's update.


## [0.4.0]

### Added

- Icon for files modified outside emacs (from Font Awesome).
- Icon for files that are being modified by another user (from Font
  Awesome).
- Icicle minor mode icon.
- Auto complete minor mode icon (from Font Awesome).
- Another icon for buffers with Mac OS line-endings (from IcoMoon
  Free).
- Flyspell minor mode icon (from IcoMoon Free).
- Ergoemacs minor mode icon (from Font Awesome).
- Messages mode icon (from Font Awesome).
- Conf mode icon (from Font Awesome).
- Fundamental mode icon (from Font Awesome).
- Dockerfile mode icon.
- Drag stuff minor mode icon (from Font Awesome).
- Helm minor mode icon.
- Javascript-IDE mode (js2-mode) icon.
- Auto indent minor mode (from Font Awesome)
- Automatic setup of arbitrary font, currently still support
  IcoMoon-Free, FontAwesome, font-mfizz and github-octicons.

### Changed

- Mode names in mode icon specifications are now case-sensitive.
- All mode names in mode icon specifications have changed to only
  match the complete name, not a partial name.
- C, C++, C#, PHP and Java modes match a wider variety of modes.  See
  (Issue #18).
- Allow black and white xpm icons to match the mode-line face colors
  (both active and inactive, for major and minor modes)
- Allow displaying the major mode name in the mode-line only, or both
  the mode-line and in other buffers, like ibuffer.
  - If the `mode-name` variable is changed, then anything that looks
    at this variable will pick up the icon.
  - This can be customized by the variable
    `mode-icons-change-mode-name`.
  - This should be modified for packages like `powerline` and
    `smart-mode-line`.
- Allow desaturating and matching the mode-line face colors for xpm
  images.
- Allow emojis to be used as mode-icons
- Allow font icons to be used without the font.
- Add gimp inferior buffer to generate images for mode-icons,
  preferring the xpm format.

### Removed

- The option to hide major mode names.

## [0.3.0]

### Added
- Support for using Octicons, Font Awesome and Font Mfizz as icons.
- The following icons were added:
  - Rainbow mode icon.
  - Scala mode icon (from Font Mfizz).
  - Auto revert mode icon.
  - Markdown mode icon.
  - Magit mode icon (from Font Awesome).
  - magit-gh-pulls minor mode icon (from Font Awesome).
  - Zip-Archive mode icon (from Font Awesome).
  - Calc and Calculator icons (from Font Awesome).
  - A General debug mode icon (from Font Awesome).
  - Calendar icon (from Font Awesome).
  - C mode icon (from Font Mfizz).
  - Help mode icon (from Font Awesome).
  - Woman mode icon (from Font Awesome).
  - Custom mode icon (from Font Awesome).
  - Golden ratio mode icon.
  - BibTeX mode icon.
  - C++ mode icon (from Font Mfizz).
  - C# mode icon (from Font Mfizz).
  - Elixir mode icon (from Font Mfizz).
  - Erlang mode icon (from Font Mfizz).
  - Haskell mode icon (from Font Mfizz).
  - Clojure mode icon (from Font Mfizz).
  - Java mode icon (from Font Mfizz).
  - Perl and CPerl mode icons (from Font Mfizz).
  - Octave mode icon.
  - Autohotkey mode icon.
  - SAS and BUGS icons for R mode.
  - Info mode icon (from Font Awesome).
  - Icon for narrowed buffers (from Font Awesome).
  - Icon for read-only buffers (from Font Awesome).
  - Icon for writable buffers (from Font Awesome).
  - Icon for unsaved buffers (from Font Awesome).
  - Icon for buffers with Mac OS line-endings (from Font Awesome).
  - Icon for buffers with Windows line-endings (from Font Awesome).
  - Icon for buffers with Unix line-endings (from Font Mfizz).
  - Text mode icon (from Font Awesome).
  - Icon for buffers with undecided encoding (from Font Awesome).
- Support for using Jpeg files as icons.
- The following minor mode lighters have been hidden:
  - Flyspell minor mode lighter.
  - Org-Indent minor mode lighter.
  - Isearch minor mode lighter.
- Support for keeping the mode name visible next to the icon.
- The following customization options were added:
  - Major mode base text properties.
  - Narrow text properties.
  - Read only text properties.
  - Modified text properties.
  - EOL text.
  - EOL space.
  - Modified status space.
  - Read-only space.
  - Show mode name.

### Changed

- The default value of the mode icons customization option was changed
  to include the new icons.
- The possible values for mode icons were changed to include the newly
  supported options.

## [0.2.0]

### Added
- CSS mode icon.
- Coffee mode icon.
- Compilation mode icon.
- Emacs Speaks Statistics (R) mode icon.
- Haml mode icon.
- Image mode icon for ImageMagick.
- Inf-Ruby mode icon.
- JavaScript mode icon.
- Projectile Rails Server mode icon.
- Ruby mode icon.
- Sass/Scss mode icon.
- Shell-script mode icon.
- Slim mode icon.
- Snippet mode icon.
- Term mode icon.
- Web mode icon.
- XML mode icon.
- YAML mode icon.
- YASnippet minor-mode icon.
- nXML mode icon.
- Showing icons for minor modes.
- Go mode icon.
- Customization options.
- Lisp Interaction mode icon.

[Unreleased]: https://github.com/ryuslash/mode-icons/compare/0.4.0...HEAD
[0.4.0]: https://github.com/ryuslash/mode-icons/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/ryuslash/mode-icons/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/ryuslash/mode-icons/compare/0.1.0...0.2.0
