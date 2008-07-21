#ifndef SHERPA_PATH_HXX
#define SHERPA_PATH_HXX

/*
 * Copyright (c) 2007, The EROS Group, LLC. All rights reserved.
 * Copyright (c) 2004, The EROS Group, LLC and Johns Hopkins
 * University. All rights reserved.
 * 
 * This software was developed to support the EROS secure operating
 * system project (http://www.eros-os.org). The latest version of
 * the OpenCM software can be found at http://www.opencm.org.
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials
 *    provided with the distribution.
 * 
 * 3. Neither the name of the The EROS Group, LLC nor the name of
 *    Johns Hopkins University, nor the names of its contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <string>
#include <dirent.h>
#include "GCPtr.hxx"

namespace sherpa {
  /// @brief Encapsulation of an open directory.
  ///
  /// This class exists primarily to ensure that underlying file 
  /// pointers to open directories get properly closed when their scope
  /// exits, regardless of <em>how</em> it exits.
  class OpenDir : public Countable {
    friend class Path;

    DIR* dir;

    /// Create an OpenDir encapsulation object, which then has all
    /// responsibility for closing this directory. The DIR argument is
    /// the result returned from the opendir() libc call.  This
    /// constructor really isn't intended to be directly used. Rather,
    /// it is intended that an OpenDir object will be obtained by
    /// calling Path::OpenDir().
    OpenDir(DIR *d)
    {
      dir = d;
    }

  public:
    ~OpenDir();

    /// Read the next entry in this directory. Returns an empty string
    /// when there are no further directory entries.
    std::string readDir();

    /// Close this directory if it is not already closed.
    void close();
  };

  /// @brief Encapsulation of a file system path.
  ///
  /// Paths can be either relative (leading '/'), absolute (non-empty,
  /// not leading '/'), or empty. An empty path is a degenerate case
  /// that arises because we need the default constructor Path() to take
  /// on a well-defined value. Concatenation with empty paths yields
  /// whichever argument is non-empty. The car() and cdr() operations on
  /// empty paths return empty paths. File system operations on empty
  /// paths throw exceptions.
  ///
  /// Eventually, opencm will handle paths as UTF-8 strings, but for now
  /// we only handle 127 bit ascii encoded in 8 bit characters. Some
  /// European users have noted that this is a pain in the neck for
  /// them, but until I understand the filesystem compatibility issues
  /// better I don't want to dive in on solving this.
  ///
  /// Update: The issue with UTF-8 isn't strictly a problem with the
  /// Path class. It's a problem of interacting successfully with the
  /// filesystem interface on filesystems that do not support unicode
  /// file names. It's a client portability issue rather than a
  /// functional issue, and we probably need to consider making this a
  /// configurable thing rather than penalizing the Europeans. Linux
  /// EXT[23] at this point handle UTF-8 filenames just fine. I need to
  /// get a status update on Win32.

  class Path : public Countable {
    std::string s;

    //  Path(const char *p, size_t subStringLen);

    const char *c_suffix() const;

  public:
    Path()
      : s()
    {
    }

    /// @brief Constructs a Path object from the passed string.
    ///
    /// When we get around to doing UTF paths tihs operation will
    /// (logically) imply parsing the string to find the directory
    /// separator characters.
    Path(const std::string& str)
      : s(str)
    {
    }

    /// @brief Path object describing the current directory (i.e. '.').
    static const Path CurDir;
    /// @brief Path object describing the root of the path tree (i.e. '/').
    static const Path RootDir;
    /// @brief Relative path to parent directory from current directory
    /// (i.e. '..').
    static const Path ParentDir;

    /// @brief Concatenate two path components to form a new path (with
    /// an intervening directory separator).
    Path operator +(const Path& newTail) const; /* pathwise concatenate */
    /// @brief Append a string onto the current path (without an
    /// intervening directory separator).
    Path operator <<(const char *) const; /* append */
    /// @brief Append a string onto the current path (without an
    /// intervening directory separator).
    Path operator <<(const std::string&) const; /* append */

    /// @brief Compare two paths for equality.
    ///
    /// @bug Should this be doing a comparison of canonical paths, or a
    /// stringwise comparison?
    bool operator ==(const Path& p) const
    {
      return (s == p.s);
    }

    /// @brief Compare two paths for inequality.
    ///
    /// @bug Should this be doing a comparison of canonical paths, or a
    /// stringwise comparison?
    bool operator !=(const Path& p) const
    {
      return (s != p.s);
    }

    /// @brief Compare two paths for magnitude.
    ///
    /// @bug Should this be doing a comparison of canonical paths, or a
    /// stringwise comparison?
    bool operator <(const Path& p) const
    {
      return (s < p.s);
    }

    /// @brief Compare two paths for magnitude.
    ///
    /// @bug Should this be doing a comparison of canonical paths, or a
    /// stringwise comparison?
    bool operator >(const Path& p) const
    {
      return (s > p.s);
    }

    /// @brief Return true <em>iff</em> c is a directory separator character.
    static bool isDirSep(char c);

    /// @brief Remove the file named by this Path.
    void remove() const;
    /// @brief Remove the file named by this Path, along with any tilde
    /// (temporary) files.
    void removeEditorFile() const;

    /// @brief Rename the file associated with this path to the new name
    /// provided by newPath.
    void rename(const Path& newPath) const;
    /// @brief Rename the file associated with this path to the new name
    /// provided by newPath, creating any necessary directories as a
    /// side effect.
    void srename(const Path& newPath) const;

    /// @brief return true <em>iff</em> this path has a corresponding
    /// entity in the file system (a file, directory, or symlink).
    bool exists() const;
    /// @brief return true <em>iff</em> this path names a directory.
    bool isDir() const;
    /// @brief return true <em>iff</em> this path names a file.
    bool isFile() const;
    /// @brief return true <em>iff</em> this path names an executable file.
    bool isExecutable() const;
    /// @brief return true <em>iff</em> this path names a symbolic link.
    bool isSymLink() const;

    /// @brief return true <em>iff</em> this path is zero length.
    inline bool isEmpty() const { return s.empty(); }

    /// @brief return true <em>iff</em> this path is an absolute path.
    ///
    /// Note that isAbsolute() and isRelative() are not quite duals of
    /// each other because of the existence of empty paths. Care should
    /// be taken in constructing path logic to make sure that these
    /// tests are used appropriately.
    inline bool isAbsolute() const { return !isEmpty() && s.at(0) == '/'; }
    /// @brief return true <em>iff</em> this path is a relative path.
    ///
    /// Note that isAbsolute() and isRelative() are not quite duals of
    /// each other because of the existence of empty paths. Care should
    /// be taken in constructing path logic to make sure that these
    /// tests are used appropriately.
    inline bool isRelative() const { return !isEmpty() && s.at(0) != '/'; }

    /// @brief return true <em>iff</em> path is a prefix of another path.
    bool isPrefixOf(const Path& other) const;

    /// @brief return true <em>iff</em> path is a @em proper prefix of
    /// another path (i.e. prefix and not equal).
    inline bool isProperPrefixOf(const Path& other) const
    {
      return (!(*this == other) && isPrefixOf(other));
    }

    /// @brief Create directory corresponding to current path.
    /// @bug This should support permissions, but how to do that in a
    /// platform-neutral way?
    void mkdir() const;

    /// @brief Create directory corresponding to current path, including
    /// intervening directories.
    /// @bug This should support permissions, but how to do that in a
    /// platform-neutral way?
    void smkdir() const;

    /// Create empty file corresponding to current Path.
    /// @bug This should support permissions, but how to do that in a
    /// platform-neutral way?
    void create() const;

    /// @brief Remove directory corresponding to current path, but only
    /// if it is empty.
    void rmdir() const;
    /// @brief Remove directory corresponding to current path, including
    /// all of its contents (recursively) as necessary.
    void rmdirRecursively() const;

    /// @brief Make the file corresponding to this path be
    /// executable.
    ///
    /// @bug The concept behind this operation may be intrinsically UNIX
    /// dependent. I'm not altogether sure what to do about that.
    void setExecutable(bool) const;

    /// @brief Set the file modification time for the file corresponding
    /// to this Path.
    void setModTime(const char *isoTime) const;

    /// @brief Return the destination of this symbolic link.
    /// to this Path.
    Path resolveLink() const;

    /// @brief Return length in bytes of target file corresponding to
    /// this path.
    off_t fileLength(void) const;

    /// @brief Return true <em>iff</em> two paths both exist and both
    /// reference the same inode entry in the file system.
    bool sameAs(const Path& other) const;

    /// @brief Make Path be a new symbolic link to the target path.
    int mkSymLink(const Path& target) const;
    /// @brief Make the real file named by this Path be a symlink to the
    /// target path.
    ///
    /// By 'real file', we mean the current path if it is not a symbolic
    /// link, or the target of the symlink if the current path
    /// <em>is</em> a symbolic link.
    int linkTail(const Path& target) const;

    /// @brief Return a representation of this Path as a string.
    std::string asString() const { return s; }

    /// @brief Return a representation of this Path as a null-terminated
    /// string suitable for printf() and friends.
    ///
    /// The returned pointer is a pointer into the internal
    /// representation of the path object. Any operation that modifies
    /// the Path object may cause this pointer to become invalid. The
    /// receiver must <em>not</em> attempt to free this string.
    const char * c_str() const { return s.c_str(); }


    /* OS-specific */
    /// @brief Return the name of an platform-specific directory for the
    /// creation ofo temporary files.
    static Path scratchDir();

    /// @brief Return the name of the current working directory.
    static Path getCWD();
    /// @brief Return a temporary file name in the specified directory
    /// whose base is the specified base.
    ///
    /// This is a strongly randomized file name with a known prefix. The
    /// resulting file name is <em>not</em> a function of the current
    /// process ID.
    static Path mkTmpName(const Path& dir, const char *base);

    /// @brief Return a canonicalized path corresponding to the input
    /// path.
    ///
    /// The canonicalization removes redundancies like "././foo" and
    /// converts "../x/y" into "y". Note that in the presence of
    /// symbolic links, the latter canonicalization may <em>not</em>
    /// correspond to the state of the file system, which is a nuisance
    /// that I don't have a good solution for.
    Path canonical() const;

    /// @brief Return the first component of this path.
    ///
    /// The car of an empty path is empty.
    Path car() const;

    /// @brief Return this path without the first component.
    ///
    /// The cdr of an empty path is empty.
    Path cdr() const;

    /// @brief Return the last component of this path.
    ///
    /// The tail of an empty path is empty.
    Path tail() const;

    /// @brief Return the directory component of this path.
    ///
    /// The directory of an empty path is empty.
    Path directory() const;

    /// @brief Return the directory name of this path.
    ///
    /// The dirName of an empty path is empty.
    /// 
    /// @bug This is a temporary compatibility hack so that I don't have
    /// to change a whole lot of code right now. It should be removed.
    inline Path dirName() const
    {
      return directory();
    }

    /// @brief Return the file suffix, if any.
    ///
    /// If there is no suffix, the return value is the empty string.
    std::string suffix() const;

     /// @brief Return the file <em>less</em> the suffix, if any.
    ///
    /// If there is no suffix, the return value is the same as the input
    Path stem() const;

    /* A portable subset of the 'stat' structure */
    typedef enum {
      psUnknown = '?',		/* unknown file type */
      psChrDev =  'C',		/* character device */
      psDir =     'D',		/* directory */
      psFile =    'F',		/* regular file */
      psSymlink = 'S'		/* symbolic link */
    } portstat_filetype_t;

    struct portstat_t {
      bool                 exists;
      time_t               modTime;
      portstat_filetype_t  type;
      off_t                len;
      bool                 isExec;
    };

    /// @brief Return portable file status information about the file
    /// corresponding to this path.
    portstat_t stat() const;

    /// @brief Open the directory corresponding to this path.
    GCPtr<OpenDir> openDir(bool mustExist) const;
  
    /// @brief Return true <em>iff</em> this string is a directory entry
    /// that should be skipped when performing directory enumerations.
    static bool shouldSkipDirent(const std::string& nm);
  } ;

  inline
  std::ostream& operator<<(std::ostream& strm, const Path& p)
  {
    strm << p.asString();
    return strm;
  }

} /* namespace sherpa */
// Local Variables:
// mode:c++
// End:

#endif /* SHERPA_PATH_HXX */
