type node('a) = {
  uid: UID.t,
  tag: option('a),
  nodes: list(node('a)),
  links: list(Link.lcaPath),
  transform: Node.transform,
  bbox: Node.bbox,
  nodeRender: Node.bbox => React.element,
};

/**
 *  computes the bboxes of the child nodes of the input node
 */
module MS = Belt.Map.String;
let rec convert =
        (
          {uid, tag, nodes, renderingLinks, layoutLinks, layout, computeBBox, nodeRender}:
            LCA.node('a),
        )
        : node('a) => {
  let bboxList = List.map(convert, nodes);
  let bboxMap = List.map(({uid, bbox}) => (uid, bbox), bboxList);
  let nodeTransforms = layout(bboxMap, layoutLinks);
  // Js.log2("nodeTransforms", nodeTransforms |> MS.toArray);
  let nodeBBoxes =
    MS.merge(
      bboxMap |> Array.of_list |> MS.fromArray,
      nodeTransforms,
      (uid, Some(bbox), Some(transform)) =>
      Some(bbox->Rectangle.transform(transform))
    );
  let bbox = computeBBox(nodeBBoxes);
  if (List.length(bboxList) == MS.size(nodeBBoxes)) {
    let nodes = List.map(n => {...n, transform: MS.getExn(nodeTransforms, n.uid)}, bboxList);
    {uid, tag, nodes, links: renderingLinks, transform: Transform.ident, bbox, nodeRender};
  } else {
    Js.log2("layout function doesn't preserve nodes!", tag);
    Js.log2("bboxList", bboxList |> Array.of_list);
    Js.log2("nodeBBoxes", nodeBBoxes->MS.toArray);
    assert(false);
  };
};
