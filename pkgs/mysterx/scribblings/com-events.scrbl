#lang scribble/doc
@(require "common.rkt")

@title[#:tag "com-events"]{COM Events}

  COM events are generated by COM objects.  Unlike
  HTML events, there is no fixed set of COM
  events, though there are ``stock'' events that
  many COM objects support.  MysterX allows the
  programmer to write handlers for both stock and
  custom events.

@defproc[(com-events [obj/type (or/c com-object? com-type?)])
         (listof string?)]{

  Returns a list of strings naming the events supported by
  @racket[obj/type].

  If calling this procedure results in an error indicating that the
  COM object's coclass is ambiguous, try using either
  @racket[set-coclass!] or @racket[set-coclass-from-progid!], then
  retry @racket[com-events].}


@defproc[(com-event-type [obj/type (or/c com-object? com-type?)]
                         [ev string?])
         (listof string?)]{

  Returns the type of an event handler for the
  event @racket[ev] generated by the particular
  COM object/type @racket[obj/type].  The return type of
  all COM event handlers is void.

  See also @racket[com-events] for dealing with a COM
  object that has an ambiguous class.}

@defproc[(com-register-event-handler 
          [obj com-object?]
          [ev string?]
          [f (any/c . -> . any)])
         void?]{

  Registers @racket[f] as event handler for the event @racket[ev] when
  generated by @racket[obj].  The type of argument supplied to
  @racket[f] depends on the event; the result of @racket[f] is always
  discarded.

  See also @racket[com-events] for dealing with a COM
  object that has an ambiguous class.}

@defproc[(com-unregister-event-handler [obj com-object?] 
                                       [ev string?])
         void?]{

  Unregisters any event handler for the event @racket[ev] that is
  generated by the COM object @racket[obj].}
