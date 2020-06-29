type linkRender = option((~source: Node.bbox, ~target: Node.bbox) => React.element);

type t = {
  source: UID.t,
  target: UID.t,
  linkRender,
};

type layout = {
  source: UID.t,
  target: UID.t,
};

type lcaPath = {
  lca: UID.t,
  source: Path.t,
  target: Path.t,
  linkRender,
};

let fromLCAPath = ({source, target, linkRender}: lcaPath): t => {
  source: List.nth(source, 0),
  target: List.nth(target, 0),
  linkRender,
};