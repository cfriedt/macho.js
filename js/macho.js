// Copyright (c) 2021 Friedt Professional Engineering Services, Inc
// SPDX-License-Identifier: MIT

function noop() {
}

function align_up(val, align) {
    return (val + align - 1) & ~(align - 1);
}

function toInt(array, offset, n) {
    var x = 0;
    for (var i = 0; i < n; ++i) {
        x |= array[offset + i] << (i * 8);
    }
    return x;
}

function toUint(array, offset, n) {
    var x = 0;
    for (var i = 0; i < n; ++i) {
        x |= array[offset + i] << (i * 8);
    }
    x >>>= 0;
    return x;
}

function toInt16(array, offset) {
    return toInt(array, offset, 2);
}

function toInt32(array, offset) {
    return toInt(array, offset, 4);
}

function toInt64(array, offset) {
    return toInt(array, offset, 8);
}

function toUint16(array, offset) {
    return toUint(array, offset, 2);
}

function toUint32(array, offset) {
    return toUint(array, offset, 4);
}

function toUint64(array, offset) {
    return toUint(array, offset, 8);
}

function isspace(c) {
    const space = new Set([' '.charCodeAt(), '\f'.charCodeAt(), '\n'.charCodeAt(), '\r'.charCodeAt(), '\t'.charCodeAt(), '\v'.charCodeAt()]);
    return space.has(c);
}

function rstrip(x) {
    var y = x;
    while (y.length > 0) {
        var c = y[y.length - 1];
        if (c == '\0'.charCodeAt() || isspace(c)) {
            y = y.slice(0, -1);
            continue;
        }
        break;
    }
    return y;
}

function read_cstring_from_array(array, offs) {
    var s = '';
    for (var i = offs; array[i] != '\0'.charCodeAt(); ++i) {
        s += String.fromCharCode(array[i]);
    }
    return s;
}

function enum_str(type, value) {
    for (var k in type) if (type[k] == value) return k;
    return null;
}

function to_mask(type) {
    var mask = 0;
    for (var v in type) {
        mask |= v;
    }
    return mask;
}

const MHMagic = {
    // Constant for the magic field of the mach_header (32-bit architectures)
    MH_MAGIC: 0xfeedface,  // the mach magic number
    MH_CIGAM: 0xcefaedfe,  // NXSwapInt(MH_MAGIC)

    // Constant for the magic field of the mach_header (32-bit architectures)
    MH_MAGIC_64: 0xfeedfacf, // the 64-bit mach magic number
    MH_CIGAM_64: 0xcffaedfe, // NXSwapInt(MH_MAGIC_64)
};

const MHCpuMask = {
    CPU_ARCH_MASK: 0xff000000, // mask for architecture bits
    CPU_SUBTYPE_MASK: 0xff000000, // mask for feature flags
    CPU_SUBTYPE_LIB64: 0x80000000, // 64 bit libraries
    // pointer authentication with versioned ABI
    CPU_SUBTYPE_PTRAUTH_ABI: 0x80000000,
    CPU_ARCH_ABI64: 0x01000000, // 64 bit ABI
    CPU_ARCH_ABI64_32: 0x02000000, // ABI for 64-bit hardware with 32-bit types; LP32
    CPU_TYPE_ANY: -1,
    CPU_SUBTYPE_ANY: -1,
    CPU_SUBTYPE_MULTIPLE: -1,
};

const MHCpuType = {
    CPU_SUBTYPE_LITTLE_ENDIAN: 0,
    CPU_SUBTYPE_BIG_ENDIAN: 1,

    CPU_TYPE_VAX: 1,
    CPU_TYPE_MC680x0: 6,
    CPU_TYPE_X86: 7,
    CPU_TYPE_I386: 7 /* CPU_TYPE_X86 */,
    CPU_TYPE_X86_64: 7 /* CPU_TYPE_X86 */ | 0x01000000 /* CPU_ARCH_ABI64 */,
    CPU_TYPE_MIPS: 8,
    CPU_TYPE_MC98000: 10,
    CPU_TYPE_HPPA: 11,
    CPU_TYPE_ARM: 12,
    CPU_TYPE_ARM64: 12 /* CPU_TYPE_ARM */ | 0x01000000 /* CPU_ARCH_ABI64 */,
    CPU_TYPE_ARM64_32: 12 /* CPU_TYPE_ARM */ | 0x02000000 /* CPU_ARCH_ABI64_32 */,
    CPU_TYPE_MC88000: 13,
    CPU_TYPE_SPARC: 14,
    CPU_TYPE_I860: 15,
    CPU_TYPE_ALPHA: 16,
    CPU_TYPE_POWERPC: 18,
    CPU_TYPE_POWERPC64: 18 /* CPU_TYPE_POWERPC */ | 0x01000000 /* CPU_ARCH_ABI64 */,
};

const MHFileType = {
    // Constants for the filetype field of the mach_header
    MH_OBJECT: 0x1,  // relocatable object file
    MH_EXECUTE: 0x2,  // demand paged executable file
    MH_FVMLIB: 0x3,  // fixed VM shared library file
    MH_CORE: 0x4,  // core file
    MH_PRELOAD: 0x5,  // preloaded executable file
    MH_DYLIB: 0x6,  // dynamically bound shared library
    MH_DYLINKER: 0x7,  // dynamic link editor
    MH_BUNDLE: 0x8,  // dynamically bound bundle file
    MH_DYLIB_STUB: 0x9,  // shared library stub for linking only, no section contents
    MH_DSYM: 0xa,  // companion file with only debug sections
    MH_KEXT_BUNDLE: 0xb,  // x86_64 kexts
};

const MHFlag = {
    // Constants for the flags field of the mach_header
    MH_NOUNDEFS: 0x1,  // the object file has no undefined references
    MH_INCRLINK: 0x2,  // the object file is the output of an incremental link against a base file and can't be link edited again
    // the object file is input for the dynamic linker and can't be staticly link edited again
    MH_DYLDLINK: 0x4,
    // the object file's undefined references are bound by the dynamic linker when loaded
    MH_BINDATLOAD: 0x8,
    MH_PREBOUND: 0x10,  // the file has its dynamic undefined references prebound
    MH_SPLIT_SEGS: 0x20,  // the file has its read-only and read-write segments split
    // the shared library init routine is to be run lazily via catching memory faults to its writeable segments (obsolete)
    MH_LAZY_INIT: 0x40,
    MH_TWOLEVEL: 0x80,  // the image is using two-level name space bindings
    // the executable is forcing all images to use flat name space bindings
    MH_FORCE_FLAT: 0x100,
    MH_NOMULTIDEFS: 0x200,  // this umbrella guarantees no multiple defintions of symbols in its sub-images so the two-level namespace hints can always be used
    // do not have dyld notify the prebinding agent about this executable
    MH_NOFIXPREBINDING: 0x400,
    // the binary is not prebound but can have its prebinding redone. only used when MH_PREBOUND is not set
    MH_PREBINDABLE: 0x800,
    MH_ALLMODSBOUND: 0x1000,  // indicates that this binary binds to all two-level namespace modules of its dependent libraries. only used when MH_PREBINDABLE and MH_TWOLEVEL are both set
    // safe to divide up the sections into sub-sections via symbols for dead code stripping
    MH_SUBSECTIONS_VIA_SYMBOLS: 0x2000,
    MH_CANONICAL: 0x4000,  // the binary has been canonicalized via the unprebind operation
    MH_WEAK_DEFINES: 0x8000,  // the final linked image contains external weak symbols
    MH_BINDS_TO_WEAK: 0x10000,  // the final linked image uses weak symbols
    // When this bit is set, all stacks in the task will be given stack execution privilege.  Only used in MH_EXECUTE filetypes
    MH_ALLOW_STACK_EXECUTION: 0x20000,
    // When this bit is set, the binary declares it is safe for use in processes with uid zero
    MH_ROOT_SAFE: 0x40000,
    // When this bit is set, the binary declares it is safe for use in processes when issetugid() is true
    MH_SETUID_SAFE: 0x80000,
    // When this bit is set on a dylib, the linker does not need to examine dependent dylibs to see if any are re-exported
    MH_NO_REEXPORTED_DYLIBS: 0x100000,
    MH_PIE: 0x200000,  // When this bit is set, the OS will load the main executable at a random address.  Only used in MH_EXECUTE filetypes
    MH_DEAD_STRIPPABLE_DYLIB: 0x400000,  // Only for use on dylibs.  When linking against a dylib that has this bit set, the linker will automatically not create a LC_LOAD_DYLIB load command to the dylib if no symbols are being referenced from the dylib
    // Contains a section of type S_THREAD_LOCAL_VARIABLES
    MH_HAS_TLV_DESCRIPTORS: 0x800000,
    // When this bit is set, the OS will run the main executable with a non-executable heap even on platforms(e.g. i386) that don't require it. Only used in MH_EXECUTE filetypes
    MH_NO_HEAP_EXECUTION: 0x1000000,
    // The code was linked for use in an application extension
    MH_APP_EXTENSION_SAFE: 0x0200000,
};

class MachHeader {
    constructor(magic, cpu_type, cpu_subtype, file_type, ncmds, sizeofcmds, flags, reserved = 0) {
        this.magic = magic;
        this.cpu_type = cpu_type;
        this.cpu_subtype = cpu_subtype;
        this.file_type = file_type;
        this.ncmds = ncmds;
        this.sizeofcmds = sizeofcmds;
        this.flags = flags;
        this.reserved = reserved;

        if (!Object.values(MHMagic).includes(magic)) {
            throw 'invalid Mach Header magic ' + magic.toString(16);
        }
        if (!Object.values(MHCpuType).includes(cpu_type)) {
            throw 'invalid Mach Header cpu type ' + cpu_type.toString(16);
        }
        if (!Object.values(MHCpuType).includes(cpu_subtype)) {
            throw 'invalid Mach Header cpu subtype ' + cpu_subtype.toString(16);
            //console.log('invalid Mach Header cpu subtype ' + cpu_subtype.toString(16));
        }
        if (!Object.values(MHFileType).includes(file_type)) {
            throw 'invalid Mach Header file type ' + file_type.toString(16);
        }
        if (flags & to_mask(MHFlag) != 0) {
            throw 'invalid Mach Header flags ' + flags.toString(16);
        }
    }

    get_byte_order() {
        if (this.magic == MHMagic.MH_CIGAM || this.magic == MHMagic.MH_CIGAM_64) {
            return 'big';
        } else {
            return 'little';
        }
    }

    is_64() {
        return this.magic == MHMagic.MH_CIGAM_64 || this.magic == MHMagic.MH_MAGIC_64;
    }

    get_align() {
        return this.is_64() ? 8 : 4;
    }

    get size() {
        return this.is_64() ? 32 : 28;
    }

    static deserialize(input, callback = noop) {
        let array = input.array.slice(input.offset);
        if (array.length < 28) {
            throw 'array of length ' + array.length + ' is too small for a Mach Header!';
        }

        // FIXME: this is all little-endian. There does not seem to be a consistent way to do this in JS :(
        var magic = toUint32(array, 0);
        var cpu_type = toUint32(array, 4);
        var cpu_subtype = toUint32(array, 8);
        var file_type = toUint32(array, 12);
        var ncmds = toUint32(array, 16);
        var sizeofcmds = toUint32(array, 20);
        var flags = toUint32(array, 24);

        var reserved = 0;
        if (magic == MHMagic.MH_MAGIC_64 || magic == MHMagic.MH_CIGAM_64) {
            if (array.length < 32) {
                throw 'array of length ' + array.length + ' is too small for a 64-bit Mach Header!';
            }
            reserved = toUint32(array, 28);
        }

        var mh = new MachHeader(magic, cpu_type, cpu_subtype, file_type, ncmds, sizeofcmds, flags, reserved);
        callback(input.array, input.offset, input.offset + mh.size, mh);
        input.offset += mh.size;

        return mh;
    }

    format() {
        var obj = Object.assign({}, this);
        obj.magic = enum_str(MHMagic, obj.magic);
        obj.cpu_type = enum_str(MHCpuType, obj.cpu_type);
        obj.cpu_subtype = enum_str(MHCpuType, obj.cpu_subtype);
        obj.file_type = enum_str(MHFileType, obj.file_type);
        var flags = [];
        Object.values(MHFlag).forEach((f) => {
            if ((obj.flags & f) != 0) {
                flags.push(enum_str(MHFlag, f));
            }
        });
        obj.flags = flags;
        return obj;
    }

    toString() {
        return JSON.stringify(this.format(), null, ' ').replaceAll('"', '');
    }
}

class LCGeneric {
    constructor(cmd, size) {
        this.cmd = cmd;
        this.size = size;
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);
        const cmd = toUint32(array, 0);
        const size = toUint32(array, 4);
        var lc = new LCGeneric(cmd, size);
        callback(input, input.offset, input.offset + size, lc);
        input.offset += size;
        return lc;
    }

    format() {
        var obj = Object.assign({}, this);
        obj.cmd = enum_str(LoadCommand, obj.cmd);
        return obj;
    }

    toString() {
        var obj = this.format();
        return JSON.stringify(obj, null, ' ').replaceAll('"', '');
    }
}

const SGFlag = {
    // Constants for the flags field of the segment_command
    SG_HIGHVM: 0x1,  // the file contents for this segment is for the high part of the VM space, the low part is zero filled(for stacks in core files)
    SG_FVMLIB: 0x2,  // this segment is the VM that is allocated by a fixed VM library, for overlap checking in the link editor
    SG_NORELOC: 0x4,  // this segment has nothing that was relocated in it and nothing relocated to it, that is it maybe safely replaced without relocation
    SG_PROTECTED_VERSION_1: 0x8,  // This segment is protected.  If the segment starts at file offset 0, the first page of the segment is not protected.  All other pages of the segment are protected
    SG_READ_ONLY: 0x10,  // This segment is made read-only after fixups
};

class LCSegment extends LCGeneric {
    constructor(cmd, size, segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, nsects, flags) {
        super(cmd, size);
        this.segname = segname;
        this.vmaddr = vmaddr;
        this.vmsize = vmsize;
        this.fileoff = fileoff;
        this.filesize = filesize;
        this.maxprot = maxprot;
        this.initprot = initprot;
        this.nsects = nsects;
        this.flags = flags;
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);

        var n;
        var lc;
        var o = 16;
        var cmd = toUint32(array, 0);

        switch (cmd) {
            case LoadCommand.LC_SEGMENT:
                n = 4;
                break;
            case LoadCommand.LC_SEGMENT_64:
                n = 8;
                break;
            default:
                throw `Expected LCSegment or LCSegment64 but found ${enum_str(LoadCommand, cmd)}!`;
        }

        var size = toUint32(array, 4);
        var segname = String.fromCharCode(...rstrip(array.slice(8, 16)));
        var vmaddr = toUint(array, o, n); o += n;
        var vmsize = toUint(array, o, n); o += n;
        var fileoff = toUint(array, o, n); o += n;
        var filesize = toUint(array, o, n); o += n;
        var maxprot = toUint32(array, o); o += 4;
        var initprot = toUint32(array, o); o += 4;
        var nsects = toUint32(array, o); o += 4;
        var flags = toUint32(array, o); o += 4;

        // NOTE: this is not documented in the spec!!~!!
        var nsects_trailing = toUint(array, o, n); o += n;

        switch (cmd) {
            case LoadCommand.LC_SEGMENT:
                lc = new LCSegment(cmd, size, segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, nsects, flags);
                break;
            case LoadCommand.LC_SEGMENT_64:
                lc = new LCSegment64(cmd, size, segname, vmaddr, vmsize, fileoff, filesize, maxprot, initprot, nsects, flags);
                break;
            default:
                throw `Expected LCSegment or LCSegment64 but found ${enum_str(LoadCommand, cmd)}!`;
        }

        if (callback != noop) {
            // callback for the Segment
            callback(input.array, input.offset, input.offset + o, lc);

            var sect;
            var input2 = {
                array: input.array,
                offset: input.offset + o,
            };

            // callback for section
            for (var i = 0; i < nsects_trailing; ++i) {
                switch (cmd) {
                    case LoadCommand.LC_SEGMENT:
                        sect = Section.deserialize(input2, callback);
                        break;
                    case LoadCommand.LC_SEGMENT_64:
                        sect = Section64.deserialize(input2, callback);
                        break;
                    default:
                        throw 'Expected LCSegment or LCSegment64!';
                }
            }
        }
        input.offset += size;

        return lc;
    }

    format() {
        var obj = super.format();
        obj.vmaddr = '0x' + obj.vmaddr.toString(16);
        obj.fileoff = '0x' + obj.fileoff.toString(16);
        obj.maxprot = '0x' + obj.maxprot.toString(16);
        obj.initprot = '0x' + obj.initprot.toString(16);
        var flags = [];
        Object.values(MHFlag).forEach((f) => {
            if ((obj.flags & f) != 0) {
                flags.push(enum_str(SGFlag, f));
            }
        });
        obj.flags = flags;
        return obj;
    }
}

class LCSegment64 extends LCSegment {
    static deserialize(input, callback = noop) {
        return LCSegment.deserialize(input, callback);
    }
}

class LCSymtab extends LCGeneric {
    constructor(symoff, nsyms, stroff, strsize) {
        super(LoadCommand.LC_SYMTAB, 24);
        this.symoff = symoff;
        this.nsyms = nsyms;
        this.stroff = stroff;
        this.strsize = strsize;
    }

    format() {
        var obj = super.format();
        obj.symoff = '0x' + obj.symoff.toString(16);
        obj.stroff = '0x' + obj.stroff.toString(16);
        return obj;
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);

        var symoff = toUint32(array, 8);
        var nsyms = toUint32(array, 12);
        var stroff = toUint32(array, 16);
        var strsize = toUint32(array, 20);

        var lc = new LCSymtab(symoff, nsyms, stroff, strsize);

        if (callback != noop) {
            // callback for the symtab Load Command
            callback(input.array, input.offset, input.offset + 24, lc);

            var input2 = {
                array: input.array,
                offset: symoff,
                stroff: stroff,
                strsize: strsize,
            };

            for (var i = 0; i < nsyms; ++i) {
                var sym = NList.deserialize(input2, callback);
            }

            // callback for the string table
            callback(input.array, stroff, stroff + strsize, 'String Table');
        }

        input.offset += 24;
        return lc;
    }
}

class LCDSymtab extends LCGeneric {
    constructor(ilocalsym, nlocalsym, iextdefsym, nextdefsym,
        iundefsym, nundefsym, tocoff, ntoc, modtaboff, nmodtab,
        indirectsymoff, nindirectsym, extreloff, nextrel,
        locreloff, nlocrel) {

        super(LoadCommand.LC_DYSYMTAB, 80);
        this.ilocalsym = ilocalsym;
        this.nlocalsym = nlocalsym;
        this.iextdefsym = iextdefsym;
        this.nextdefsym = nextdefsym;
        this.iundefsym = iundefsym;
        this.nundefsym = nundefsym;
        this.tocoff = tocoff;
        this.ntoc = ntoc;
        this.modtaboff = modtaboff;
        this.nmodtab = nmodtab;
        this.indirectsymoff = indirectsymoff;
        this.nindirectsym = nindirectsym;
        this.extreloff = extreloff;
        this.nextrel = nextrel;
        this.locreloff = locreloff;
        this.nlocrel = nlocrel;
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);

        var o = 8;
        var ilocalsym = toUint32(array, o); o += 4;
        var nlocalsym = toUint32(array, o); o += 4;
        var iextdefsym = toUint32(array, o); o += 4;
        var nextdefsym = toUint32(array, o); o += 4;
        var iundefsym = toUint32(array, o); o += 4;
        var nundefsym = toUint32(array, o); o += 4;
        var tocoff = toUint32(array, o); o += 4;
        var ntoc = toUint32(array, o); o += 4;
        var modtaboff = toUint32(array, o); o += 4;
        var nmodtab = toUint32(array, o); o += 4;
        var indirectsymoff = toUint32(array, o); o += 4;
        var nindirectsym = toUint32(array, o); o += 4;
        var extreloff = toUint32(array, o); o += 4;
        var nextrel = toUint32(array, o); o += 4;
        var locreloff = toUint32(array, o); o += 4;
        var nlocrel = toUint32(array, o); o += 4;

        var lc = new LCDSymtab(ilocalsym, nlocalsym, iextdefsym, nextdefsym,
            iundefsym, nundefsym, tocoff, ntoc, modtaboff, nmodtab,
            indirectsymoff, nindirectsym, extreloff, nextrel,
            locreloff, nlocrel);

        if (callback != noop) {
            callback(input.array, input.offset, input.offset + o, lc);
        }

        input.offset += o;
        return lc;
    }
}

class LCLoadDylib {
    static deserialize(input, callback = noop) {
        return LCGeneric.deserialize(input, callback);
    }
}

class LCDyldInfo {
    static deserialize(input, callback = noop) {
        return LCGeneric.deserialize(input, callback);
    }
}

class LCBuiildVersion {
    static deserialize(input, callback = noop) {
        return LCGeneric.deserialize(input, callback);
    }
}

const LoadCommand = {
    // Constants for the cmd field of all load commands, the type
    LC_REQ_DYLD: 0x80000000,

    LC_SEGMENT: 0x1,  // segment of this file to be mapped
    LC_SYMTAB: 0x2,  // link-edit stab symbol table info
    LC_SYMSEG: 0x3,  // link-edit gdb symbol table info (obsolete)
    LC_THREAD: 0x4,  // thread
    LC_UNIXTHREAD: 0x5,  // unix thread (includes a stack)
    LC_LOADFVMLIB: 0x6,  // load a specified fixed VM shared library
    LC_IDFVMLIB: 0x7,  // fixed VM shared library identification
    LC_IDENT: 0x8,  // object identification info (obsolete)
    LC_FVMFILE: 0x9,  // fixed VM file inclusion (internal use)
    LC_PREPAGE: 0xa,  // prepage command (internal use)
    LC_DYSYMTAB: 0xb,  // dynamic link-edit symbol table info
    LC_LOAD_DYLIB: 0xc,  // load a dynamically linked shared library
    LC_ID_DYLIB: 0xd,  // dynamically linked shared lib ident
    LC_LOAD_DYLINKER: 0xe,  // load a dynamic linker
    LC_ID_DYLINKER: 0xf,  // dynamic linker identification
    LC_PREBOUND_DYLIB: 0x10,  // modules prebound for a dynamically lin ked shared library
    LC_ROUTINES: 0x11,  // image routines
    LC_SUB_FRAMEWORK: 0x12,  // sub framework
    LC_SUB_UMBRELLA: 0x13,  // sub umbrella
    LC_SUB_CLIENT: 0x14,  // sub client
    LC_SUB_LIBRARY: 0x15,  // sub library
    LC_TWOLEVEL_HINTS: 0x16,  // two-level namespace lookup hints
    LC_PREBIND_CKSUM: 0x17,  // prebind checksum
    // load a dynamically linked shared library that is allowed to be missing (all symbols are weak imported)
    LC_LOAD_WEAK_DYLIB: 0x18 | 0x80000000 /* LC_REQ_DYLD */,
    LC_SEGMENT_64: 0x19,  // 64-bit segment of this file to be mapped
    LC_ROUTINES_64: 0x1a,  // 64-bit image routines
    LC_UUID: 0x1b,  // the uuid
    LC_RPATH: 0x1c | 0x80000000 /* LC_REQ_DYLD */,  // runpath additions
    LC_CODE_SIGNATURE: 0x1d,  // local of code signature
    LC_SEGMENT_SPLIT_INFO: 0x1e,  // local of info to split segments
    LC_REEXPORT_DYLIB: 0x1f | 0x80000000 /* LC_REQ_DYLD */,  // load and re-export dylib
    LC_LAZY_LOAD_DYLIB: 0x20,  // delay load of dylib until first use
    LC_ENCRYPTION_INFO: 0x21,  // encrypted segment information
    LC_DYLD_INFO: 0x22,  // compressed dyld information
    // compressed dyld information only
    LC_DYLD_INFO_ONLY: 0x22 | 0x80000000 /* LC_REQ_DYLD */,
    LC_LOAD_UPWARD_DYLIB: 0x23 | 0x80000000 /* LC_REQ_DYLD */,  // load upward dylib
    LC_VERSION_MIN_MACOSX: 0x24,  // build for MacOSX min OS version
    LC_VERSION_MIN_IPHONEOS: 0x25,  // build for iPhoneOS min OS version
    LC_FUNCTION_STARTS: 0x26,  // compressed table of function start addresses
    LC_DYLD_ENVIRONMENT: 0x27,  // string for dyld to treat like environment variable
    LC_MAIN: 0x28 | 0x80000000 /* LC_REQ_DYLD */,  // replacement for LC_UNIXTHREAD
    LC_DATA_IN_CODE: 0x29,  // table of non-instructions in __text
    LC_SOURCE_VERSION: 0x2A,  // source version used to build binary
    LC_DYLIB_CODE_SIGN_DRS: 0x2B,  // Code signing DRs copied from linked dylibs
    LC_ENCRYPTION_INFO_64: 0x2C,  // 64-bit encrypted segment information
    LC_LINKER_OPTION: 0x2D,  // linker options in MH_OBJECT files
    LC_LINKER_OPTIMIZATION_HINT: 0x2E,  // optimization hints in MH_OBJECT files
    LC_VERSION_MIN_TVOS: 0x2F,  // build for AppleTV min OS version
    LC_VERSION_MIN_WATCHOS: 0x30,  // build for Watch min OS version
    LC_NOTE: 0x31,  // arbitrary data included within a Mach-O file
    LC_BUILD_VERSION: 0x32,  // build for platform min OS version

    // used with linkedit_data_command, payload is trie
    LC_DYLD_EXPORTS_TRIE: 0x33 | 0x80000000 /* LC_REQ_DYLD */,
    // used with linkedit_data_command
    LC_DYLD_CHAINED_FIXUPS: 0x34 | 0x80000000 /* LC_REQ_DYLD */,
    LC_FILESET_ENTRY: 0x35 | 0x80000000 /* LC_REQ_DYLD */,  // used with fileset_entry_command

    deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);
        if (array.length < 8) {
            throw 'array length ' + array.length + ' too small for load command!';
        }

        var cmd_type = toUint32(array, 0);
        var cmd_size = toUint32(array, 4);

        if (array.length < cmd_size) {
            throw 'array length ' + array.length + ' too small for load command of size ' + cmd_size + '!';
        }

        if (!Object.values(LoadCommand).includes(cmd_type)) {
            throw 'invalid command type ' + cmd_type.toString(16) + '!';
        }

        switch (cmd_type) {
            case LoadCommand.LC_SEGMENT:
                return LCSegment.deserialize(input, callback);
            case LoadCommand.LC_SEGMENT_64:
                return LCSegment64.deserialize(input, callback);
            case LoadCommand.LC_SYMTAB:
                return LCSymtab.deserialize(input, callback);
            case LoadCommand.LC_DYSYMTAB:
                return LCDSymtab.deserialize(input, callback);
            case LoadCommand.LC_LOAD_DYLIB:
            case LoadCommand.LC_LOAD_WEAK_DYLIB:
            case LoadCommand.LC_REEXPORT_DYLIB:
            case LoadCommand.LC_LAZY_LOAD_DYLIB:
            case LoadCommand.LC_LOAD_UPWARD_DYLIB:
                return LCLoadDylib.deserialize(input, callback);
            case LoadCommand.LC_DYLD_INFO:
            case LoadCommand.LC_DYLD_INFO_ONLY:
                return LCDyldInfo.deserialize(input, callback);
            case LoadCommand.LC_BUILD_VERSION:
                return LCBuiildVersion.deserialize(input, callback);
            default:
                return LCGeneric.deserialize(input, callback);
        }
    },
};

const SectionFlagsMask = {
    SECTION_TYPE: 0x000000ff,  // 256 section types
    SECTION_ATTRIBUTES: 0xffffff00,  // 24 section attributes
    SECTION_ATTRIBUTES_USR: 0xff000000,  // User setable attributes
    SECTION_ATTRIBUTES_SYS: 0x00ffff00,  // system settable attributes
};

const SectionType = {
    // Constants for the type of a section
    S_REGULAR: 0x0,  // regular section
    S_ZEROFILL: 0x1,  // zero fill on demand section
    S_CSTRING_LITERALS: 0x2,  // section with only literal C strings
    S_4BYTE_LITERALS: 0x3,  // section with only 4 byte literals
    S_8BYTE_LITERALS: 0x4,  // section with only 8 byte literals
    S_LITERAL_POINTERS: 0x5,  // section with only pointers to literals */

    // For the two types of symbol pointers sections and the symbol stubs section
    // they have indirect symbol table entries.  For each of the entries in the
    // section the indirect symbol table entries, in corresponding order in the
    // indirect symbol table, start at the index stored in the reserved1 field
    // of the section structure.  Since the indirect symbol table entries
    // correspond to the entries in the section the number of indirect symbol table
    // entries is inferred from the size of the section divided by the size of the
    // entries in the section.  For symbol pointers sections the size of the entries
    // in the section is 4 bytes and for symbol stubs sections the byte size of the
    // stubs is stored in the reserved2 field of the section structure.

    S_NON_LAZY_SYMBOL_POINTERS: 0x6,  // section with only non-lazy symbol pointers
    S_LAZY_SYMBOL_POINTERS: 0x7,  // section with only lazy symbol pointers
    // section with only symbol stubs, byte size of stub in the reserved2 field
    S_SYMBOL_STUBS: 0x8,
    // section with only function pointers for initialization
    S_MOD_INIT_FUNC_POINTERS: 0x9,
    // section with only function pointers for termination
    S_MOD_TERM_FUNC_POINTERS: 0xa,
    S_COALESCED: 0xb,  // section contains symbols that are to be coalesced
    // zero fill on demand section (that can be larger than 4 gigabytes)
    S_GB_ZEROFILL: 0xc,
    S_INTERPOSING: 0xd,  // section with only pairs of function pointers for interposing
    S_16BYTE_LITERALS: 0xe,  // section with only 16 byte literals
    S_DTRACE_DOF: 0xf,  // section contains DTrace Object Format
    // section with only lazy symbol pointers to lazy loaded dylibs
    S_LAZY_DYLIB_SYMBOL_POINTERS: 0x10,

    // Section types to support thread local variables
    S_THREAD_LOCAL_REGULAR: 0x11,  // template of initial values for TLVs
    S_THREAD_LOCAL_ZEROFILL: 0x12,  // template of initial values for TLVs
    S_THREAD_LOCAL_VARIABLES: 0x13,  // TLV descriptors
    S_THREAD_LOCAL_VARIABLE_POINTERS: 0x14,  // pointers to TLV descriptors
    // functions to call to initialize TLV values
    S_THREAD_LOCAL_INIT_FUNCTION_POINTERS: 0x15,
    S_INIT_FUNC_OFFSETS: 0x16,  // 32-bit offsets to initializers
};

const SectionAttribute = {
    // Constants for the section attributes part of the flags field of a section structure.
    // section contains only true machine instructions
    S_ATTR_PURE_INSTRUCTIONS: 0x80000000,
    // section contains coalesced symbols that are not to be in a ranlib table of contents
    S_ATTR_NO_TOC: 0x4000000,
    // ok to strip symbols in this section in files with the MH_DYLDLINK flag
    S_ATTR_STRIP_STATIC_SYMS: 0x20000000,
    S_ATTR_NO_DEAD_STRIP: 0x10000000,  // no dead stripping
    S_ATTR_LIVE_SUPPORT: 0x08000000,  // blocks are live if they reference live blocks
    // Used with i386 code stubs written on by dyld
    S_ATTR_SELF_MODIFYING_CODE: 0x04000000,

    // If a segment contains any sections marked with S_ATTR_DEBUG then all
    // sections in that segment must have this attribute.  No section other than
    // a section marked with this attribute may reference the contents of this
    // section.  A section with this attribute may contain no symbols and must have
    // a section type S_REGULAR.  The linker will not copy section contents
    // from sections with this attribute into its output file.  These sections
    // generally contain DWARF debugging info.

    S_ATTR_DEBUG: 0x02000000,  // a debug section
    // section contains some machine instructions
    S_ATTR_SOME_INSTRUCTIONS: 0x00000400,
    S_ATTR_EXT_RELOC: 0x00000200,  // section has external relocation entries
    S_ATTR_LOC_RELOC: 0x00000100,  // section has local relocation entries
};

const RTypeGeneric = {
    GENERIC_RELOC_VANILLA: 0,
    GENERIC_RELOC_PAIR: 1,
    GENERIC_RELOC_SECTDIFF: 2,
    GENERIC_RELOC_PB_LA_PTR: 3,
    GENERIC_RELOC_LOCAL_SECTDIFF: 4,
    GENERIC_RELOC_TLV: 5,
};

class Reloc {
    constructor(r_address, r_symbolnum, r_pcrel, r_length, r_extern, r_type) {
        this.type = 'Reloc';
        this.r_address = r_address;
        this.r_symbolnum = r_symbolnum;
        this.r_pcrel = r_pcrel;
        this.r_length = r_length;
        this.r_extern = r_extern;
        this.r_type = r_type;
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);

        var r_address = toInt32(array, 0);
        var x = toUint32(array, 4);

        var r_symbolnum = x & ((1 << 24) - 1);
        var r_pcrel = (x >> 24) & 1;
        var r_length = (x >> 25) & 3;
        var r_extern = (x >> 27) & 1;
        var r_type = (x >> 28) & 0xf;

        var rel = new Reloc(r_address, r_symbolnum, r_pcrel, r_length, r_extern, r_type);
        callback(input.array, input.offset, input.offset + 8, rel);

        input.offset += 8;
        return rel;
    }

    format() {
        var obj = Object.assign({}, this);
        obj.r_type = enum_str(RTypeGeneric, obj.r_type);
        return obj;
    }

    toString() {
        var obj = this.format();
        return JSON.stringify(obj, null, ' ').replaceAll('"', '');
    }
}

class Section {
    constructor(sectname, segname, addr, size, offset, align, reloff, nreloc, flags, reserved1, reserved2) {
        this.type = 'Section';
        this.sectname = sectname;
        this.segname = segname;
        this.addr = addr;
        this.size = size;
        this.offset = offset;
        this.align = align;
        this.reloff = reloff;
        this.nreloc = nreloc;
        this.flags = flags;
        this.reserved1 = reserved1;
        this.reserved2 = reserved2;
    }

    static deserialize(input, callback = noop, n = 4) {
        const array = input.array.slice(input.offset);

        if (!(n == 4 || n == 8)) {
            throw `Excpected n == 4 or n == 8 but received n == ${n}!`;
        }

        var sect;
        var o = 32;
        var sectname = String.fromCharCode(...rstrip(array.slice(0, 16)));
        var segname = String.fromCharCode(...rstrip(array.slice(16, 32)));
        var addr = toUint(array, o, n); o += n;
        var size = toUint(array, o, n); o += n;
        var offset = toUint32(array, o); o += 4;
        var align = toUint32(array, o); o += 4;
        var reloff = toUint32(array, o); o += 4;
        var nreloc = toUint32(array, o); o += 4;
        var flags = toUint32(array, o); o += 4;
        var reserved1 = toUint32(array, o); o += 4;
        var reserved2 = toUint32(array, o); o += 4;
        if (n == 8) {
            var reserved3 = toUint32(array, o); o += 4;
            sect = new Section64(sectname, segname, addr, size, offset, align, reloff, nreloc, flags, reserved1, reserved2, reserved3);
        } else {
            sect = new Section(sectname, segname, addr, size, offset, align, reloff, nreloc, flags, reserved1, reserved2);
        }

        if (callback != noop) {
            // callback for the section header
            callback(input.array, input.offset, input.offset + o, sect);

            // callback for the section data
            var obj = `${segname},${sectname} data`;
            callback(input.array, offset, offset + size, obj);

            var input2 = {
                array: input.array,
                offset: reloff,
            };

            // callback for relocations
            for (var i = 0; i < nreloc; ++i) {
                var rel = Reloc.deserialize(input2, callback);
            }
        }

        input.offset += o;
        return sect;
    }

    format() {
        var obj = Object.assign({}, this);
        obj.addr = '0x' + obj.addr.toString(16);
        obj.offset = '0x' + obj.offset.toString(16);
        obj.reloff = '0x' + obj.reloff.toString(16);
        var flags = [enum_str(SectionType, SectionFlagsMask.SECTION_TYPE & obj.flags)];
        Object.values(SectionAttribute).forEach((f) => {
            if ((obj.flags & f) != 0) {
                flags.push(enum_str(SectionAttribute, f));
            }
        });
        obj.flags = flags;
        return obj;
    }

    toString() {
        var obj = this.format();
        return JSON.stringify(obj, null, ' ').replaceAll('"', '');
    }
}

class Section64 extends Section {
    constructor(sectname, segname, addr, size, offset, align, reloff, nreloc, flags, reserved1, reserved2, reserved3) {
        super(sectname, segname, addr, size, offset, align, reloff, nreloc, flags, reserved1, reserved2);
        this.type = 'Section64';
        this.reserved3 = reserved3;
    }

    static deserialize(input, callback = noop) {
        return Section.deserialize(input, callback, 8);
    }
}


// The n_type field really contains four fields:
//	unsigned char N_STAB:3,
//		      N_PEXT:1,
//		      N_TYPE:3,
//		      N_EXT:1;
// which are used via the following masks.

const NLTypeMask = {
    N_STAB: 0xe0,  // if any of these bits set, a symbolic debugging entry
    N_PEXT: 0x10,  // private external symbol bit
    N_TYPE: 0x0e,  // mask for the type bits
    N_EXT: 0x01,  // external symbol bit, set for external symbols
};

// Values for N_TYPE bits of the n_type field.


const NLType = {
    N_UNDF: 0x0,  // undefined, n_sect == NO_SECT
    N_ABS: 0x2,  // absolute, n_sect == NO_SECT
    N_SECT: 0xe,  // defined in section number n_sect
    N_PBUD: 0xc,  // prebound undefined (defined in a dylib)
    N_INDR: 0xa,  // indirect
};

const NLStab = {
    N_GSYM: 0x20,  // global symbol: name,,NO_SECT,type,0
    N_FNAME: 0x22,  // procedure name (f77 kludge): name,,NO_SECT,0,0
    N_FUN: 0x24,  // procedure: name,,n_sect,linenumber,address
    N_STSYM: 0x26,  // symbol: name,,n_sect,type,address
    N_LCSYM: 0x28,  // .lcomm symbol: name,,n_sect,type,address
    N_BNSYM: 0x2e,  // begin nsect sym: 0,,n_sect,0,address
    N_AST: 0x32,  // AST file path: name,,NO_SECT,0,0
    N_OPT: 0x3c,  // emitted with gcc2_compiled and in gcc source
    N_RSYM: 0x40,  // register sym: name,,NO_SECT,type,register
    N_SLINE: 0x44,  // src line: 0,,n_sect,linenumber,address
    N_ENSYM: 0x4e,  // end nsect sym: 0,,n_sect,0,address
    N_SSYM: 0x60,  // structure elt: name,,NO_SECT,type,struct_offset
    N_SO: 0x64,  // source file name: name,,n_sect,0,address
    N_OSO: 0x66,  // object file name: name,,(see below),0,st_mtime
    // historically N_OSO set n_sect to 0. The N_OSO
    // n_sect may instead hold the low byte of the
    // cpusubtype value from the Mach-O header. */
    N_LSYM: 0x80,  // local sym: name,,NO_SECT,type,offset
    N_BINCL: 0x82,  // include file beginning: name,,NO_SECT,0,sum
    N_SOL: 0x84,  // included file name: name,,n_sect,0,address
    N_PARAMS: 0x86,  // compiler parameters: name,,NO_SECT,0,0
    N_VERSION: 0x88,  // compiler version: name,,NO_SECT,0,0
    N_OLEVEL: 0x8A,  // compiler -O level: name,,NO_SECT,0,0
    N_PSYM: 0xa0,  // parameter: name,,NO_SECT,type,offset
    N_EINCL: 0xa2,  // include file end: name,,NO_SECT,0,0
    N_ENTRY: 0xa4,  // alternate entry: name,,n_sect,linenumber,address
    N_LBRAC: 0xc0,  // left bracket: 0,,NO_SECT,nesting level,address
    N_EXCL: 0xc2,  // deleted include file: name,,NO_SECT,0,sum
    N_RBRAC: 0xe0,  // right bracket: 0,,NO_SECT,nesting level,address
    N_BCOMM: 0xe2,  // begin common: name,,NO_SECT,0,0
    N_ECOMM: 0xe4,  // end common: name,,n_sect,0,0
    N_ECOML: 0xe8,  // end common (local name): 0,,n_sect,0,address
    N_LENG: 0xfe,  // second stab entry with length information
};

class NList {
    constructor(n_name, n_type, n_sect, n_desc, n_value) {
        this.type = 'NList';
        this.n_name = n_name;
        this.n_type = n_type;
        this.n_sect = n_sect;
        this.n_desc = n_desc;
        this.n_value = n_value;
    }

    format() {
        var obj = Object.assign({}, this);
        var n_type = [];
        if (NLTypeMask.N_STAB & obj.n_type != 0) {
            n_type.push(enum_str(NLStab, NLTypeMask.N_STAB & obj.n_type));
        }
        if (NLTypeMask.N_PEXT & obj.n_type != 0) {
            n_type.push(enum_str(NLTypeMask, NLTypeMask.N_PEXT));
        }
        if (NLTypeMask.N_TYPE & obj.n_type != 0) {
            n_type.push(enum_str(NLType, NLTypeMask.N_TYPE & obj.n_type));
        }
        if (NLTypeMask.N_EXT & obj.n_type != 0) {
            n_type.push(enum_str(NLTypeMask, NLTypeMask.N_EXT));
        }
        obj.n_type = n_type;
        obj.n_value = '0x' + obj.n_value.toString(16);
        return obj;
    }

    toString() {
        return JSON.stringify(this.format(), null, ' ').replaceAll('"', '');
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);
        // FIXME: need to communicate this properly
        const n = 8;

        if (!(n == 4 || n == 8)) {
            throw `Excpected n == 4 or n == 8 but received n == ${n}!`;
        }

        var nl;
        var n_name = toUint32(array, 0);
        var n_type = array[4];
        var n_sect = array[5];
        var n_desc = toUint16(array, 6);
        var n_value = toUint(array, 8, n);

        n_name = read_cstring_from_array(input.array, input.stroff + n_name);

        if (n == 4) {
            nl = new NList(n_name, n_type, n_sect, n_desc, n_value);
        } else {
            nl = new NList64(n_name, n_type, n_sect, n_desc, n_value);
        }

        callback(input.array, input.offset, input.offset + 8 + n, nl);

        input.offset += 8 + n;
        return nl;
    }
}

class NList64 extends NList {
    constructor(n_name, n_type, n_other, n_desc, n_value) {
        super(n_name, n_type, n_other, n_desc, n_value);
        this.type = 'NList64';
    }

    static deserialize(input, callback = noop) {
        return NList.deserialize(input, callback, 8);
    }
}

class MachOFile {
    get_sections() {
    }

    get_symtab() {
    }

    get_strtab() {
    }

    get_load_commands() {
    }

    get_segments() {
    }

    get_text_segment() {
    }

    constructor(mach_header, load_commands = [], data = []) {
        this.mach_header = mach_header;
        this.load_commands = load_commands;
        this.data = data;
    }

    static deserialize(input, callback = noop) {
        const array = input.array.slice(input.offset);
        const mh = MachHeader.deserialize(input, callback);

        if (mh.sizeofcmds - mh.size > array.length) {
            throw 'Mach Header command size exceeds array length!';
        }

        var lcs = [];
        for (var n = mh.ncmds, m = mh.sizeofcmds; n > 0 && m > 0; --n) {
            var lc = LoadCommand.deserialize(input, callback);
            m -= lc.size;
            lcs.push(lc);
            input.offset = align_up(input.offset, mh.get_align());
        }

        const data = input.array.slice(input.offset);
        var mf = new MachOFile(mh, lcs, data);
        return mf;
    }

    serialize() {
        return [];
    }
}
