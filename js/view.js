// Copyright (c) 2021 Friedt Professional Engineering Services, Inc
// SPDX-License-Identifier: MIT

let current_machofile_idx = 0;
// name of each machofile
let machofile_names = [];
// map of name => RenderedMachOFile
let machofile_renders = new Map();
let machofile_contents = new Map();

function fromHexString(hexString) {
    return new Uint8Array(hexString.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));
}

function isgraph(c) {
    return '!'.charCodeAt() <= c && c <= '~'.charCodeAt();
}

function randomcolor() {
    const nbits = 24;
    const max = 1 << nbits;
    return Math.floor(Math.random() * max);
}

class RegionOfInterest {
    constructor(begin, end, obj) {
        this.begin = begin;
        this.end = end;
        // FIXME: find a better way to decide on colors
        this.color = randomcolor();
        this.bg_color = ~this.color & ((1 << 24) - 1);
        this.obj = obj;
    }
}

function onMouseOverContents(roi_id) {
    var begin = parseInt(roi_id.substr(4));
    var filename = machofile_names[current_machofile_idx];
    var rend = machofile_renders[filename];
    var roi = rend.rois.get(begin);
    var el = document.getElementById('machofile_hover');
    el.innerHTML = roi.obj.toString();
}

class RenderedMachOfile {
    // n := number of bytes in file / array
    static render_offsets(n) {
        var html = '';
        for (var i = 0; i < n; i += 16) {
            html += ("00000000" + i.toString(16)).substr(-8) + ':<br/>';
        }
        return html;
    }

    render_contents(contents) {
        var html = '';
        // FIXME: likely better to iterate over sorted ROIs
        for (var i = 0, n = contents.length, roi = null, roi_len = -1; i < n; ++i) {
            if (i % 16 == 0 && i != 0) {
                html += '<br/>';
            } else if (i % 2 == 0 && i != 0) {
                html += ' ';
            }

            if (roi == null) {
                if (this.rois.has(i)) {
                    roi = this.rois.get(i);
                    roi_len = roi.end - roi.begin;
                    html += '<span ';
                    html += `id="roi_${i}" `;
                    html += `style="color:#${roi.color.toString(16)};`;
                    html += `background-color:#${roi.bg_color.toString(16)}" `
                    html += `onmouseover="onMouseOverContents('roi_${i}')">`;
                }
            }

            html += ("00" + contents[i].toString(16)).substr(-2);

            if (roi != null) {
                --roi_len;
                if (roi_len == 0) {
                    html += '</span>';
                    roi = null;
                }
            }
        }
        return html;
    }

    render_ascii(contents) {
        var html = '';
        for (var i = 0, n = contents.length; i < n; ++i) {
            if (i % 16 == 0 && i != 0) {
                html += '<br/>';
            }

            if (' '.charCodeAt() == contents[i] || isgraph(contents[i])) {
                html += String.fromCharCode(contents[i]);
            } else {
                html += '.';
            }
        }

        return html;
    }

    constructor(contents) {

        var input = {
            offset: 0,
            array: contents,
        };

        var last_offset = 0;
        this.rois = new Map();

        const callback = (array, begin, end, obj) => {
            var roi = new RegionOfInterest(begin, end, obj);
            this.rois.set(begin, roi);
        };

        var mf = MachOFile.deserialize(input, callback);
        this.offsets = RenderedMachOfile.render_offsets(contents.length);
        this.contents = this.render_contents(contents);
        this.ascii = this.render_ascii(contents);
    }
}

async function fetch_machofile_contents(name) {
    const uri = '/api/input?name=' + encodeURIComponent(name);
    let response = await fetch(uri);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    var obj = await response.json();
    return obj.contents;
}

function render_current_machofile() {
    if (current_machofile_idx >= machofile_names.length) {
        return;
    }

    let name = machofile_names[current_machofile_idx];
    var name_div = document.getElementById('machofile_name');
    name_div.innerHTML = `${name} (${current_machofile_idx + 1} of ${machofile_names.length})`;

    fetch_machofile_contents(name).then( contents => {

        if (!machofile_contents[name] || machofile_contents[name] != contents) {
            try {
                machofile_renders[name] = new RenderedMachOfile(fromHexString(contents));
                machofile_contents[name] = contents;
            } catch(e) {
                console.log(`Failed to render Mach-O file ${machofile_names[current_machofile_idx]}`);
                return;
            }
        }

        let rend = machofile_renders[name];

        var offsets_div = document.getElementById('machofile_offsets');
        var contents_div = document.getElementById('machofile_contents');
        var ascii_div = document.getElementById('machofile_ascii');
        var hover_div = document.getElementById('machofile_hover');

        offsets_div.innerHTML = rend.offsets;
        contents_div.innerHTML = rend.contents;
        ascii_div.innerHTML = rend.ascii;
        hover_div.innerHTML = '';
    });
}

function machofile_change_and_render(delta) {
    const n = machofile_names.length;
    var i = current_machofile_idx;

    if (n == 0 || delta == 0) {
        return;
    }

    i = (i + delta + n) % n;

    current_machofile_idx = i;
    render_current_machofile();
}

function machofile_prev() {
    machofile_change_and_render(-1);
}

function machofile_next() {
    machofile_change_and_render(+1);
}

function render_machofiles(names) {
    machofile_names = names ? names : [];
    current_machofile_idx = 0;
    render_current_machofile();
}

async function fetch_machofile_names() {
    let response = await fetch('/api/input');
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    var obj = await response.json();
    return obj.names;
}

function onLoad() {
    fetch_machofile_names().then(names => render_machofiles(names));
}

document.addEventListener('DOMContentLoaded', onLoad);

document.querySelector('#machofile_prev').addEventListener('click', machofile_prev);
document.querySelector('#machofile_next').addEventListener('click', machofile_next);
document.addEventListener('keydown', (event) => {
    switch (event.code) {
        case 'KeyP':
            machofile_prev();
            break;
        case 'KeyN':
            machofile_next();
            break;
        default:
            // console.log(`key: ${event.name}, code: ${event.code}`);
            break;
    }
}, false);
