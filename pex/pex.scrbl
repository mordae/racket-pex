#lang scribble/manual

@require[(for-label pex)
         (for-label typed/racket/base)
         (for-label typed/racket/class)]

@title{Apollo Art PEx Power Control}
@author+email["Jan Dvořák" "mordae@anilinux.org"]


@defmodule[pex]

@(define pex-link "http://www.apolloart.cz/index.php?jazyk=en&sekce=produkty")

This library allows for full control of PEx devices from
@link[pex-link]{Apollo Art}. These devices provide power switching and fading
capabilities and are typically installed in large, intelligent rooms.


@section{Interface}

@defidform[Bank%]{
  Type of the @racket[bank%] class.
}

@defclass[bank% object% ()]{
  Collection of relays and faders on a single bus.

  @defconstructor[((path Path-String) (id Positive-Integer))]{
    Connect to the bus using a serial port device on given @racket[path]
    and scan bank with given @racket[id] for any relays or faders.
  }

  @defmethod[(command (head String) (body String)) String]{
    Execute a raw PEx BUS command.
    Please refer to the manual for more information.
  }

  @defmethod[(list-relays) (Listof Positive-Integer)]{
    List identifiers of relays found on the bus, within given bank.
  }

  @defmethod[(list-faders) (Listof Positive-Integer)]{
    List identifiers of faders found on the bus, within given bank.
  }

  @defmethod[(get-relay (id Positive-Integer)) (Instance Relay%)]{
    Return an object representing specified relay.
  }

  @defmethod[(get-fader (id Positive-Integer)) (Instance Fader%)]{
    Return an object representing specified fader.
  }
}

@defidform[Relay%]{
  Type of the @racket[relay%] class.
}

@defclass[relay% object% ()]{
  Represents a single relay switch from given bank.

  @defconstructor[((bank (Instance Bank%)) (id Positive-Integer))]{}

  @defmethod[(get-status) String]{
    Retrieve internal relay status string.
  }

  @defmethod[(get-on?) Boolean]{
    Retrieve relay power status.
  }

  @defmethod[(set-on! (on? Boolean)) Void]{
    Modify relay power status.
  }
}

@defidform[Fader%]{
  Type of the @racket[fader%] class.
}

@defclass[fader% object% ()]{
  Represents a single fader from given bank.

  @defconstructor[((bank (Instance Bank%)) (id Positive-Integer))]{}

  @defmethod[(get-status) String]{
    Retrieve internal fader status string.
  }

  @defmethod[(get-level) Natural]{
    Get fader power level ranging from @racket[0] to @racket[99].
  }

  @defmethod[(set-level! (level Natural)) Void]{
    Set fader power level ranging from @racket[0] to @racket[99].
    Larger @racket[level] is treated as @racket[99].
  }

  @defmethod[(fade-to-level! (level Natural)) Void]{
    Fade power level from current value to the new @racket[level] with
    duration of one second.

    Fader may ignore some values inside the range, so experiment and learn
    what values work for your system. It is generally not possible to fade
    out to @racket[0]. You need to fade out to e.g. @racket[20] and then
    @racket[(send a-fader set-level! 0)].
  }
}

@; vim:set ft=scribble sw=2 ts=2 et:
