module MS = Belt.Map.String;

/* TODO: not sure whether GlobalTransform should belong to layout or renderLayout. Figure out what makes sense for animation and do that. Leaning towards former, because cleanly separates layout from rendering, and because it works well with animations, which need to compare global transforms. */
let layout = (n: KernelIR.node('a)): LayoutIR.node('a) =>
  n |> LCA.convert |> ToLayout.lower |> GlobalTransform.convert;
/* apply transitions between these */
let renderLayout = (n: LayoutIR.node('a)): React.element =>
  ToSVG.lower(n);

let render = (n: KernelIR.node('a)): React.element =>
  n
  |> LCA.convert
  |> ToLayout.lower
  |> GlobalTransform.convert
  |> ToSVG.lower;

/* TODO: not sure where this should go */
let defaultLayout = (bboxes, _) =>
  List.map(((uid, _)) => (uid, Transform.ident), bboxes) |> Array.of_list |> MS.fromArray;