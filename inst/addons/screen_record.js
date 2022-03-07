// Development and testing
/*
const video = document.getElementById("video");
const start = document.getElementById("start");
const stop = document.getElementById("stop");
start.onclick = function (e) { startRecording(); };
// Note: `stop` event is binded after the handle has been created below.
*/


// Reference: https://dev.to/sebastianstamm/screen-recording-in-10-lines-of-vanilla-js-3bo8
// Usage:
// - Call the function explicitly to share screen recording
// - Click anywhere on the page to stop the screen recording.
//   You should always check that the recording "red dot" has disappeared
//   at the tab and the screen recording notification box is gone.
async function startRecording() {
    const stream = await navigator.mediaDevices.getDisplayMedia({
        video: { mediaSource: "screen" }
    });

    const recorder = new MediaRecorder(stream);

    // Bind stop event
    // stop.onclick = function(e) {  // for development and testing
    document.querySelector("html").onclick = function(e) {
        stream.getTracks().forEach(track => track.stop());
        recorder.stop()
    };

    const chunks = [];
    recorder.ondataavailable = e => chunks.push(e.data);

    console.log("Start recording")
    recorder.start();

    recorder.onstop = e => {
        console.log("Stop recording")
        const completeBlob = new Blob(chunks, { type: chunks[0].type });
        // video.src = URL.createObjectURL(completeBlob);  // for development and testing
        saveBlob({
            'content': completeBlob,
            'name': 'animate_screenshot.mp4'
        });
    };
}


// Reference: https://stackoverflow.com/questions/19327749/javascript-blob-filename-without-link
function saveBlob(file) {  // file := {content: blob, name: string}
    if (navigator.msSaveBlob) {  // For ie and Edge
        return navigator.msSaveBlob(file.content, file.name);
    } else {
        let link = document.createElement('a');
        link.href = window.URL.createObjectURL(file.content);
        link.download = file.name;
        document.body.appendChild(link);
        link.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true, view: window}));
        link.remove();
        window.URL.revokeObjectURL(link.href);
    }
}
