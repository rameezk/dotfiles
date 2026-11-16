import { View, div } from "gpui-kit";
import { terminals, setBadge, checking } from "herdr";

export default class AgentBadge extends View {
  init() {
    this.shown = undefined;
  }

  render() {
    const blocked = terminals().filter((a) => a.status === "blocked").length;
    const text = blocked > 0 ? String(blocked) : null;
    if (text !== this.shown && !checking()) {
      this.shown = text;
      setBadge(text);
    }
    return div().w(0).h(0);
  }
}
