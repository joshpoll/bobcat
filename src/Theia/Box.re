module MS = Belt.Map.String;

/* TODO: transform must include scaling! */
let mk = (~uid=?, ~tag=?, ~dx=0., ~dy=0., node, links) => {
  open Rectangle;
  let nodeRender = bbox => {
    <rect
      x={Js.Float.toString(bbox->Rectangle.x1)}
      y={Js.Float.toString(bbox->Rectangle.y1)}
      width={Js.Float.toString(bbox->Rectangle.width)}
      height={Js.Float.toString(bbox->Rectangle.height)}
      fillOpacity="0"
      stroke="#000"
    />;
  };
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes=[node],
    ~links,
    ~layout=Kernel.defaultLayout,
    ~computeBBox=bs => bs->MS.valuesToArray->Array.to_list->union_list->inflate(dx, dy),
    ~nodeRender,
  );
};