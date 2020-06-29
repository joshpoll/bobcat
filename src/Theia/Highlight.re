module MS = Belt.Map.String;

let mk = (~uid=?, ~tag=?, ~fill, node, links) => {
  let nodeRender = bbox => {
    <rect
      x={Js.Float.toString(bbox->Rectangle.x1)}
      y={Js.Float.toString(bbox->Rectangle.y1)}
      width={Js.Float.toString(bbox->Rectangle.width)}
      height={Js.Float.toString(bbox->Rectangle.height)}
      fill
    />;
  };
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes=[node],
    ~links,
    ~layout=Kernel.defaultLayout,
    ~computeBBox=bs => bs->MS.valuesToArray->Array.to_list->Rectangle.union_list,
    ~nodeRender,
  );
};