const loadYoutubeIframe = (id, button) => {
  const iframeContainer = button.parentNode;

  const iframe = document.createElement("iframe");

  iframe.src = `https://www.youtube-nocookie.com/embed/${id}`;
  iframe.width = 320;
  iframe.height = 180;
  iframe.frameborder = 0;
  iframe.allowFullscreen = true;

  iframeContainer.appendChild(iframe);
  button.parentNode.removeChild(button);
};
