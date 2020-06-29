module MS = Belt.Map.String;

/* TODO: not sure whether GlobalTransform should belong to layout or renderLayout. Figure out what makes sense for animation and do that. Leaning towards former, because cleanly separates layout from rendering, and because it works well with animations, which need to compare global transforms. */
let layout = (n: KernelIR.node('a)): GlobalTransform.node('a) =>
  n |> LCA.convert |> Layout.convert |> GlobalTransform.convert;
/* apply transitions between these */
let renderLayout = (n: GlobalTransform.node('a)): React.element =>
  n |> RenderLinks.convert |> Render.convert;

let render = (n: KernelIR.node('a)): React.element =>
  n
  |> LCA.convert
  |> Layout.convert
  |> GlobalTransform.convert
  |> RenderLinks.convert
  |> Render.convert;

/* TODO: not sure where this should go */
let defaultLayout = (bboxes, _) =>
  List.map(((uid, _)) => (uid, Transform.ident), bboxes) |> Array.of_list |> MS.fromArray;