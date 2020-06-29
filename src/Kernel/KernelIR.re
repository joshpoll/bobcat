module MS = Belt.Map.String;

type node('a) = {
  uid: UID.t,
  tag: option('a),
  nodes: list(node('a)),
  links: list(Link.t),
  layout: (list((UID.t, Node.bbox)), list(Link.layout)) => MS.t(Node.transform),
  computeBBox: MS.t(Node.bbox) => Node.bbox,
  nodeRender: Node.bbox => React.element,
};

let mk = (~uid=?, ~tag=?, ~nodes, ~links, ~layout, ~computeBBox, ~nodeRender) => {
  uid:
    switch (uid) {
    | None => "autogen__" ++ string_of_int(UID.mk())
    | Some(uid) => uid
    },
  tag,
  nodes,
  links,
  layout,
  computeBBox,
  nodeRender,
};