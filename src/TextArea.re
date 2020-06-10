[@react.component]
let make = (~text) => {
  <textarea
    readOnly=true
    style={ReactDOMRe.Style.make(~width="700px", ~height="600px", ())}
    value=text>
  </textarea>;
};