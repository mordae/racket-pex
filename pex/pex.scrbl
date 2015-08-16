#lang scribble/manual

@require[(for-label pex)
         (for-label typed/racket/base)]

@title{Apollo Art PEx System Control}
@author+email["Jan Dvořák" "mordae@anilinux.org"]


@defmodule[pex]

@(define pex-link "http://www.apolloart.cz/index.php?jazyk=en&sekce=produkty")

This library allows for full control of PEx devices from
@link[pex-link]{Apollo Art}. These devices provide power switching and fading
capabilities and are typically installed in large, intelligent rooms.


@section{Interface}


@; vim:set ft=scribble sw=2 ts=2 et:
