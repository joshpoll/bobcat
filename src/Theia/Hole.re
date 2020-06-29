let mk = (~uid=?, ~tag=?, ()) =>
  Atom.mk(
    ~uid?,
    ~tag?,
    ~links=[],
    <rect fill="none" width="10" height="10" x="5" y="5" />,
    Rectangle.fromPointSize(~x=0., ~y=0., ~width=10., ~height=10.),
  );