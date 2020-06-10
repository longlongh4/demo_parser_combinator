type state = {
  result: string,
};

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (_, action) => {
        {result: Parser.parse(action)};
      },
      {result: ""},
    );
  let valueFromEvent = (evt): string => evt->ReactEvent.Form.target##value;
  <div>
    <input
      style={ReactDOMRe.Style.make(~width="700px", ())}
      onChange={evt => dispatch(valueFromEvent(evt))}
    />
    <hr />
    <TextArea text=state.result />
  </div>;
};