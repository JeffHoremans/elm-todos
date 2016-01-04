module Shortcut
    ( Key(..)
    , shortcut2, shortcut3
    , alt, ctrl, shift
    , ctrlAlt, ctrlShift
    , altShift, altCtrl
    ) where

{-| A library for creating shortcut signals.
Many methods are provided creating signals for keyboard combinations.
A mapping function is always passed to convert the signal to any structure wanted (e.g. to an action).
Supports a limited amount of 2- and 3-key shortcuts.  -}
import Signal
import Keyboard exposing ( isDown )
import Char exposing ( toCode )

{-| Currently supporting Ctrl, Alt and Shift as the main keys for shortcuts.  -}
type Key = Ctrl | Alt | Shift

{-| Create a 2-key shortcut constructed of a Main Key (see above) and any keyboard character key.
Uses the given mapping function to map the main key, character key and pressed state to any structure wanted.

  type Action = Shortcut Key Char Bool

  shortcut2 Shortcut Ctrl 'F'
  shortcut2 Shortcut Alt 'D'

-}
shortcut2: (Key -> Char -> Bool -> a) -> Key -> Char -> Signal a
shortcut2 f k c = Signal.map (f k c) (Signal.map2 (&&) (key k) (isDown <| toCode c))

{-| Create a 3-key shortcut similar to the 2-key method but with an additional Main Key.

  type Action = Shortcut Key Key Char Bool

  shortcut3 Shortcut Ctrl Alt 'F'
  shortcut3 Shortcut Alt Shift 'D'

-}
shortcut3: (Key -> Key -> Char -> Bool -> a ) -> Key -> Key -> Char -> Signal a
shortcut3 f k1 k2 c = Signal.map (f k1 k2 c) (Signal.map3 (\x y z -> x && y && z) (key k1) (key k2) (isDown <| toCode c))

{-| Create a 2-key shortcut using the Ctrl key as the Main Key.

  type Action = Shortcut Char Bool

  ctrl Shortcut 'F' == shortcut2 (\_ char bool -> Shortcut char bool) Ctrl 'F'

-}
ctrl : (Char -> Bool -> a) -> Char -> Signal a
ctrl f c = shortcut2withKey f Ctrl c

{-| Create a 2-key shortcut using the Alt key as the Main Key.

  type Action = Shortcut Char Bool

  alt Shortcut 'F' == shortcut2 (\_ char bool -> Shortcut char bool) Alt 'F'

-}
alt : (Char -> Bool -> a) -> Char -> Signal a
alt f c = shortcut2withKey f Alt c

{-| Create a 2-key shortcut using the Shift key as the Main Key.

  type Action = Shortcut Char Bool

  shift Shortcut 'F' == shortcut2 (\_ char bool -> Shortcut char bool) Shift 'F'

-}
shift : (Char -> Bool -> a) -> Char -> Signal a
shift f c = shortcut2withKey f Shift c

{-| Create a 3-key shortcut using the Ctrl and Alt key as the Main Keys.

  type Action = Shortcut Char Bool

  ctrlAlt Shortcut 'F' == shortcut3 (\_ _ char bool -> Shortcut char bool) Ctrl Alt 'F'

-}
ctrlAlt : (Char -> Bool -> a) -> Char -> Signal a
ctrlAlt f c = shortcut3WithKeys f Ctrl Alt c

{-| Create a 3-key shortcut using the Ctrl and Shift key as the Main Keys.

  type Action = Shortcut Char Bool

  ctrlShift Shortcut 'F' == shortcut3 (\_ _ char bool -> Shortcut char bool) Ctrl Shift 'F'

-}
ctrlShift : (Char -> Bool -> a) -> Char -> Signal a
ctrlShift f c = shortcut3WithKeys f Ctrl Shift c

{-| Create a 3-key shortcut using the Alt and Shift key as the Main Keys.

  type Action = Shortcut Char Bool

  altShift Shortcut 'F' == shortcut3 (\_ _ char bool -> Shortcut char bool) Alt Shift 'F'

-}
altShift : (Char -> Bool -> a) -> Char -> Signal a
altShift f c = shortcut3WithKeys f Alt Shift c

{-| Create a 3-key shortcut using the Alt and Ctrl key as the Main Keys.

  type Action = Shortcut Char Bool

  altCtrl Shortcut 'F' == shortcut3 (\_ _ char bool -> Shortcut char bool) Alt Ctrl 'F'

-}
altCtrl : (Char -> Bool -> a) -> Char -> Signal a
altCtrl f c = shortcut3WithKeys f Alt Ctrl c

-- Map Main key to corresponding Boolean Signal
key: Key -> Signal Bool
key key = case key of
  Ctrl  -> Keyboard.ctrl
  Alt   -> Keyboard.alt
  Shift -> Keyboard.shift

-- HELPER METHODS

-- create a 2-key shortcut, ignoring Main key while mapping
shortcut2withKey: (Char -> Bool -> a) -> Key -> Char -> Signal a
shortcut2withKey f k c = shortcut2 (\_ x y -> (f x y)) k c

-- create a 3-key shortcut, ignoring the Main keys while mapping
shortcut3WithKeys: (Char -> Bool -> a) -> Key -> Key -> Char -> Signal a
shortcut3WithKeys f k1 k2 c = shortcut3 (\_ _ x y -> (f x y)) k1 k2 c
