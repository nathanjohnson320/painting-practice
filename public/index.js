/** @typedef {{load: (Promise<unknown>); flags: (unknown)}} ElmPagesInit */

/** @type ElmPagesInit */
export default {
  load: async function (elmLoaded) {
    const app = await elmLoaded;

    console.log("App loaded", app);

    app.ports.downloadEpisode.subscribe((json) => {
      const blob = new Blob([JSON.stringify(json)], {
        type: "text/plain",
      });
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.style.display = "none";
      a.href = url;
      a.download = "episode.json";
      document.body.appendChild(a);
      a.click();
      window.URL.revokeObjectURL(url);
    });
  },
  flags: function () {
    return { currentYear: new Date().getFullYear() };
  },
};
