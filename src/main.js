import { Elm } from "./ProjectList.elm";
import json from "../projects.json";

window.initProjectList = function(node) { 
  Elm.ProjectList.init({
    node: node,
    flags: {
      projectJson: JSON.stringify(json)
    }
  });
}
