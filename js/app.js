// Copyright (c) 2021 Friedt Professional Engineering Services, Inc
// SPDX-License-Identifier: MIT

const fs = require('fs');
const http = require('http');
const path = require('path');

const ORIGIN = fs.realpathSync(path.dirname(process.argv[1]) + '/..');
const PORT = process.env.PORT || 8000;
const PATHS = get_library_search_paths();

function is_machofile(realpath) {
    let val;
    let fd = fs.openSync(realpath);
    let buf = Buffer.alloc(4);
    let bytes_read = fs.readSync(fd, buf);

    fs.closeSync(fd);
    if (bytes_read != 4) {
        return false;
    }

    // Mach-O file
    val = buf.readInt32LE() >>> 0;
    const macho_magic = [0xfeedface, 0xcefaedfe, 0xfeedfacf, 0xcffaedfe];
    if (macho_magic.includes(val)) {
        return true;
    }

    // Universal Binary
    val = buf.readInt32BE() >>> 0;
    if (val == 0xcafebabe) {
        // FIXME: support universal binaries
        return false;
    }

    return false;
}

function bfs(root, allow_exe = false, allow_frameworks = false, allow_bundles = false) {
    const EXCLUDE = new Set(['.git', '.vscode']);

    // found machofiles
    let F = [];
    // Queue of directories to visit
    let Q = [];
    // Set of visited *realpath* directories
    let V = new Set();

    Q.push(root);
    while (!Q.length == 0) {
        var node = Q.shift();
        // get 'edges' (i.e. dirents)
        fs.readdirSync(node, { withFileTypes: true }).forEach((dirent) => {
            const fakepath = node + '/' + dirent.name;
            const realpath = fs.realpathSync(fakepath);
            const relpath = fakepath.replace(root + '/', '');
            var stat;

            if (dirent.isSymbolicLink()) {
                stat = fs.statSync(realpath);
            }
            if (dirent.isFile() || (dirent.isSymbolicLink() && stat.isFile())) {
                if (is_machofile(realpath)) {
                    if (realpath.endsWith('.dylib') || realpath.endsWith('.o')) {
                        F.push(relpath);
                    } else if (allow_exe) {
                        F.push(relpath);
                    }
                } else if (realpath.endsWith('.a')) {
                    // F.push(relpath);
                }
            } else if (dirent.isDirectory() || (dirent.isSymbolicLink() && stat.isDirectory())) {
                if (EXCLUDE.has(dirent.name)) {
                    return;
                }
                if (V.has(realpath)) {
                    return;
                }

                // visit node
                V.add(realpath);

                if (realpath.endsWith('.bundle')) {
                    if (!allow_bundles) {
                        return;
                    }
                }

                if (realpath.endsWith('.framework')) {
                    if (!allow_frameworks) {
                        return;
                    }
                }

                Q.push(fakepath);
            } else {
                // skip other file types (FIFO, Block / Char devices)
            }
        });
    }

    return F;
}

function find_machofiles() {
    var machofiles = [];
    for (var i = 0; i < PATHS.length; ++i) {
        // requires ORIGIN to be first path
        var allow_exe = i == 0;
        machofiles.push(...bfs(PATHS[i], allow_exe));
    }
    return machofiles;
}

function get_library_search_paths() {
    const argv = process.argv;
    let paths = [ORIGIN];
    for (let i = 1, n = argv.length; i < n; ++i) {
        const arg = argv[i];
        if ('-L' == arg) {
            if (i >= n - 1) {
                throw 'argument "-L" must either include a path or be followed by a path';
            }
            arg = '-L' + argv[i + 1];
            ++i;
        }

        if (arg.startsWith('-L')) {
            const p = arg.substring(2);
            if (!p.startsWith('/')) {
                p = ORIGIN + '/' + p;
            }
            paths.push(fs.realpathSync(p));
        }
    }
    return paths;
}

function ext2mime(ext) {
    const map = {
        '.css': 'text/css',
        '.html': 'text/html',
        '.js': 'text/javascript'
    };
    return map[ext];
}

const server = http.createServer(async (req, res) => {
    if (req.url === '/' && req.method === 'GET') {
        req.url = '/index.html';
    }

    if (fs.existsSync(ORIGIN + '/' + req.url) && req.method === 'GET') {
        res.writeHead(200, { 'Content-Type': ext2mime(path.extname(req.url)) });
        res.write(fs.readFileSync(ORIGIN + '/' + req.url, 'utf-8'));
        res.end();
    } else if (req.url === '/api/input' && req.method === 'GET') {
        // get a list of all file names
        res.writeHead(200, { 'Content-Type': 'application/json' });
        let machofiles = find_machofiles();
        let obj = {
            names: machofiles,
        };
        res.write(JSON.stringify(obj));
        res.end();
    } else if (req.url.startsWith('/api/input?') && req.method === 'GET') {
        let params = new URLSearchParams(req.url.substring(11));
        let fakepath = params.get('name');
        let contents = null;

        for (var i = 0; i < PATHS.length; ++i) {
            let = p = PATHS[i];
            let realpath = p + '/' + fakepath;

            if (!fs.existsSync(realpath)) {
                continue;
            };

            contents = fs.readFileSync(realpath, null).toString('hex');
        }
        if (contents == null) {
            res.writeHead(404, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify({ message: `File ${fakepath} not found` }));
        } else {
            var obj = {
                contents: contents,
            };
            res.writeHead(200, { 'Content-Type': 'application/json' });
            res.write(JSON.stringify(obj));
            res.end();
        }
    } else {
        // If no route present
        res.writeHead(404, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ message: 'Route not found' }));
    }
});

server.listen(PORT, () => {
    console.log(`server started on port: ${PORT}`);
});
