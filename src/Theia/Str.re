let mk = (~uid=?, ~tag=?, s) => {
  let width = float_of_int(String.length(s) * 10);
  let height = 12.5;
  Atom.mk(
    ~uid?,
    ~tag?,
    <text
      textAnchor="middle"
      dominantBaseline="middle"
      fontFamily="courier"
      transform={
        "translate("
        ++ Js.Float.toString(width /. 2.)
        ++ ", "
        ++ Js.Float.toString(height /. 2. +. 1.5)
        ++ ")"
      }>
      {React.string(s)}
    </text>,
    Rectangle.fromPointSize(~x=0., ~y=0., ~width, ~height),
  );
};