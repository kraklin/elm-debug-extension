
export const urlToKey = (url) => {
  const urlObject = new URL(url);
  return urlObject.host;
}
