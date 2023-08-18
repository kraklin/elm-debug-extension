export const urlToKey = (url) => {
  const urlObject = new URL(url);
  return urlObject.host;
}

export const isChromeBased = () => {
   if (typeof chrome !== "undefined") {
    if (typeof browser !== "undefined") {
      return false;
    } else {
      return true;
    }
  } else {
    return false;
  }
}
