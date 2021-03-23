
# Table of Contents

1.  [Technical choices](#org21aba38)
2.  [Installation](#org9ebbf19)
3.  [A bit of history](#org61673d9)

This repository contains my personal Emacs configuration.


<a id="org21aba38"></a>

# Technical choices

-   no literate configuration
-   `use-package.el`<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>
-   `straight.el`<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>
-   `ivy` and `counsel`<sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>
-   respects the Emacs reserved keys<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup> for the user as much as possible
-   single *huge* init file
-   hooks defined preferably in the most commons/general packages


<a id="org9ebbf19"></a>

# Installation

This repo can be cloned and used as it is. Either clone and `make`, or clone and
simply launch Emacs.

Please note that I use a git sub module for my private variables and config.
They are auto-loaded by this config if available, and not mandatory.


<a id="org61673d9"></a>

# A bit of history

After 3 or 4 Emacs bankruptcy<sup><a id="fnr.5" class="footref" href="#fn.5">5</a></sup> over the course of 8 or so years, I have
decided to reset my git once more today <span class="timestamp-wrapper"><span class="timestamp">[2020-12-13 Sun]</span></span>. Not because I've
rewritten it again, but because it has been stable for almost 2 years, and it is
time for it to being clean from any personal and sensitive variables, and can
therefore be publicly shared.

I went through many configuration paradigms, in the following order:

-   A copy and paste snippets 'chaos' configuration,
-   A literate configuration,
-   A literate configuration with `use-package`<sup><a id="fnr.1.100" class="footref" href="#fn.1">1</a></sup>,
-   A literate, Spacemacs<sup><a id="fnr.6" class="footref" href="#fn.6">6</a></sup> inspired, configuration with `use-package`, `evil`,
    `which-key`, `leader-key`, etc&#x2026;
-   A plain vanilla config, heavily inspired by Oremacs<sup><a id="fnr.7" class="footref" href="#fn.7">7</a></sup>, by Oleh Krehel,
    (Abo-Abo), and Ambrevar's configuration<sup><a id="fnr.8" class="footref" href="#fn.8">8</a></sup>, using the built-in `package.el`,
    with `evil` bindings, and then back to classic bindings,
-   A `use-package` + `straight`<sup><a id="fnr.2.100" class="footref" href="#fn.2">2</a></sup> configuration, centered on a single init file.

And every combination in-between which are not worth mentioning as they were
either small iterations, or short lived.


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> <https://github.com/jwiegley/use-package>

<sup><a id="fn.2" href="#fnr.2">2</a></sup> <https://github.com/raxod502/straight.el>

<sup><a id="fn.3" href="#fnr.3">3</a></sup> <https://github.com/abo-abo/swiper>

<sup><a id="fn.4" href="#fnr.4">4</a></sup> <https://www.masteringemacs.org/article/mastering-key-bindings-emacs>

<sup><a id="fn.5" href="#fnr.5">5</a></sup> <https://www.emacswiki.org/emacs/DotEmacsBankruptcy>

<sup><a id="fn.6" href="#fnr.6">6</a></sup> <https://www.spacemacs.org/>

<sup><a id="fn.7" href="#fnr.7">7</a></sup> <https://github.com/abo-abo/oremacs>

<sup><a id="fn.8" href="#fnr.8">8</a></sup> <https://gitlab.com/ambrevar/dotfiles>
