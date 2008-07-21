/*
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

#include <errno.h>
#include <fcntl.h>
#include <string.h>

#include "Path.hxx"
#include "UExcept.hxx"
#include "CVector.hxx"
#include <vector>

namespace sherpa {
  void
  OpenDir::close()
  {
    if (dir) {
      ::closedir(dir);
      dir = 0;
    }
  }

  std::string 
  OpenDir::readDir()
  {
    struct dirent* dir_ent = readdir(dir);
    if(dir_ent == 0)
      return std::string();
    return dir_ent->d_name;
  }

  OpenDir::~OpenDir()
  {
    close();
  }

  const Path Path::CurDir(".");
  const Path Path::ParentDir("..");
  const Path Path::RootDir("/");

  bool
  Path::isDirSep(char c)
  {
    return c == '/';
  }

  bool
  Path::exists() const
  {
    portstat_t ps = stat();

    return ps.exists;
  }

  bool
  Path::isFile() const
  {
    portstat_t ps = stat();

    if (ps.exists && ps.type == psFile)
      return true;

    return false;
  }

  bool
  Path::isSymLink() const
  {
    portstat_t ps = stat();

    if (ps.exists && ps.type == psSymlink)
      return true;

    return false;
  }

  bool
  Path::isDir() const
  {
    portstat_t ps = stat();

    if (ps.exists && ps.type == psDir)
      return true;

    return false;
  }

  bool
  Path::isExecutable() const
  {
    portstat_t ps = stat();

    if (ps.exists && ps.isExec)
      return true;

    return false;
  }

  off_t
  Path::fileLength() const
  {
    portstat_t ps = stat();

    if (ps.exists)
      return ps.len;

    THROW(excpt::NoObject,
	  format("Could not stat \"%s\" or wrong file type", c_str()));
  }

  void
  Path::remove() const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot remove empty path.");

    int result = unlink(c_str());

    if (result < 0 && errno != ENOENT)
      THROW(excpt::NoAccess,
	    format("Could not remove\"%s\" (errno %d)", c_str(), errno));
  }

  void
  Path::removeEditorFile() const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot remove empty path.");

    {
      Path editorTurdFile = *this << "~";
      editorTurdFile.remove();
    }
    remove();
  }

  /* NOTE: path_rename needs to be an atomic operation, and therefore
     cannot perform the path_smkdir() for us. We should perhaps consider
     introducing path_srename() and replacing path_rename() with
     path_atomic_rename() to make this explicit. */
  void
  Path::rename(const Path &newPath) const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot rename empty path.");

    int result = ::rename(c_str(), newPath.c_str());
    if (result < 0)
      THROW(excpt::NoAccess,
	    format("Could not rename\"%s\" to \"%s\" (errno %d)", 
		   c_str(), newPath.c_str(), errno));
  }

  void
  Path::srename(const Path &newPath) const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot srename empty path.");

    newPath.directory().smkdir();
    this->rename(newPath);
  }

  Path
  Path::getCWD()
  {
#ifdef __unix__
#ifdef PATH_MAX
    int len = PATH_MAX; 
#else
    int len = 255; /* POSIX guarantees this (or does it?) */
#endif
    std::vector<char> dir;
    char *cwd;

    do {
      dir.reserve(len);
      cwd =  getcwd(&dir.front(), len);
      if (cwd == NULL) {
	if (errno == ERANGE) {
	  len *= 2;
	  dir.reserve(len);
	}
	else {
	  THROW(excpt::BadValue, format("Could not get current working directory"));
	}
      }
    } while (cwd == NULL);

    Path p(&dir.front());
    return p;
#else
#  error "path_current_directory() not implemented"
#endif
  }

  int
  Path::mkSymLink(const Path& target) const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot mksymlink on empty path.");

    return ::symlink(c_str(), target.c_str());
  }

  int
  Path::linkTail(const Path& target) const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot linkTail on empty path.");

    if (isSymLink())
      return resolveLink().tail().mkSymLink(target);
    else
      return tail().mkSymLink(target);
  }

  void
  Path::setExecutable(bool shouldBeExec) const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot setExecutable on empty path.");

    if (::chmod(c_str(), shouldBeExec ? 0755 : 0644) < 0)
      THROW(excpt::NoAccess, 
	    format("Could not change permissions on \"%s\": errno %d",
		   c_str(), errno));
  }

  Path
  Path::operator+(const Path& tail) const
  {
    if (tail.isEmpty())
      return *this;
    else if (isEmpty())
      return tail;
    else {
      std::string s = this->s;
  
      s += "/";
      s += tail.s;

      return Path(s);
    }
  }

  Path
  Path::operator<<(const char *tail) const
  {
    Path p = *this;
    p.s.append(tail);
    return p;
  }

  Path
  Path::operator<<(const std::string& tail) const
  {
    Path p = *this;
    p.s.append(tail);
    return p;
  }

  void
  Path::mkdir() const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot mkdir an empty path.");

    if (::mkdir(c_str(), 0777) < 0) {
      if (errno == EEXIST)
	THROW(excpt::ObjectExists,
	      format("Could not create directory %s (errno %d)", c_str(), errno));
      else
	THROW(excpt::NoObject,
	      format("Could not create directory %s (errno %d)", c_str(), errno));
    }
  }

  void
  Path::create() const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot create an empty path.");

    if (::creat(c_str(), 0666) < 0) {
      if (errno == EEXIST)
	THROW(excpt::ObjectExists,
	      format("Could not create file %s (errno %d)", c_str(), errno));
      else
	THROW(excpt::NoObject,
	      format("Could not create file %s (errno %d)", c_str(), errno));
    }
  }

  void
  Path::rmdir() const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot rmdir an empty path.");

    if (::rmdir(c_str()) < 0)
      THROW(excpt::NoAccess, 
	    format("Could not rmdir \"%s\": errno %d\n", c_str(), errno));
  }


  GCPtr<OpenDir>
  Path::openDir(bool must_exist) const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot opendir an empty path.");

    GCPtr<OpenDir> dir = new OpenDir(::opendir(c_str()));

    if(!dir->dir)
      {
	if(must_exist)
	  THROW(excpt::NoObject, format("Couldn't open directory '%s'", c_str()));
	else
	  return 0;
      }

    return dir;
  }

  void
  Path::rmdirRecursively() const
  {
    if (isEmpty())
      THROW(excpt::BadValue, "Cannot rmdirRecursively an empty path.");

    std::string ent;
    CVector<Path> pv;

    GCPtr<OpenDir> dir = openDir(false);

    /* If the directory isn't there, that obviously there are not any entities
       inside of it */
    if(!dir)
      return;

    while ((ent = dir->readDir()).size()) {
      if (shouldSkipDirent(ent))
	continue;

      pv.append(*this + Path(ent));
    }

    dir->close();

    for (size_t i = 0; i < pv.size(); i++) {
      Path subPath(pv[i]);

      if (subPath.isDir())
	subPath.rmdirRecursively();
      else
	subPath.remove();
    }

    rmdir();
  }

  Path
  Path::scratchDir()
  {
    if (::getenv("TMPDIR"))
      return Path(::getenv("TMPDIR"));

#ifdef _PATH_VARTMP
    return Path(_PATH_VARTMP);
#endif

#ifdef _PATH_TMP
    return Path(_PATH_TMP);
#endif

#ifdef L_tmpdir
    return Path(L_tmpdir);
#endif

#ifdef P_tmpdir
    return Path(P_tmpdir);
#endif

    if (::access("/var/tmp", W_OK) == 0)
      return Path("/var/tmp");

    if (::access("/usr/tmp", W_OK) == 0)
      return Path("/usr/tmp");

    THROW(excpt::NoObject,
	  format("Could not identify a directory for temporary files"));
  }

  bool
  Path::sameAs(const Path& p) const
  {
    if (isEmpty() or p.isEmpty())
      THROW(excpt::BadValue, "Cannot check sameness of empty paths.");

#ifdef __unix__
    struct stat cur_dir;
    struct stat parent_dir;

    if (::stat(c_str(), &cur_dir) < 0)
      return false;
    if (::stat(p.c_str(), &parent_dir) < 0)
      return false;

    if (cur_dir.st_ino == parent_dir.st_ino &&
	cur_dir.st_dev == parent_dir.st_dev)
      return true;

    return false;
#else
#  error "path_same_dir() not implemented"
#endif
  }

  bool
  Path::isPrefixOf(const Path& p) const
  {
    if (isEmpty())
      return true;
    if (p.isEmpty())
      return false;

    if (car() != p.car())
      return false;

    return cdr().isPrefixOf(p.cdr());
  }

  Path
  Path::car() const
  {
    if (isEmpty())
      return *this;

    const char *slash = c_str();
    while (*slash && !isDirSep(*slash))
      slash++;

    if (*slash == 0)
      return *this;
    else
      return Path(s.substr(0, slash - c_str()));
  }

  Path
  Path::cdr() const
  {
    if (isEmpty())
      return *this;

    const char *slash = c_str();

    while (*slash && !isDirSep(*slash))
      slash++;

    if (*slash)
      return Path(slash+1);
    else
      return Path();
  }

  Path
  Path::tail() const
  {
    if (isEmpty())
      return *this;

    Path rest = cdr();
    if (!rest.isEmpty())
      return rest.tail();
    return *this;
  }

  const char *
  Path::c_suffix() const
  {
    const char *slash = 0;
    const char *dot = 0;
    const char *start = c_str();

    for (const char *s = start; *s; s++) {
      if (*s == '/')
        slash = s;
      if (*s == '.')
        dot = s;
    }

    if (!dot)
      return 0;

    if (!slash)
      return dot;

    if ((dot - start) > (slash - start))
      return dot;

    return 0;
  }

  std::string
  Path::suffix() const
  {
    const char *suf = c_suffix();
    return suf ? std::string(suf) : std::string();
  }

  Path Path::stem() const
  {
    const char *suf = c_suffix();
    if (!suf)
      return *this;

    return Path(s.substr(0, suf - s.c_str()));
  }

  Path
  Path::canonical() const
  {
    if (isEmpty())
      return *this;

    std::string s = this->s;

    size_t ndx = 0;

    while (ndx < s.length()) {
      // Reduce "/\0" to ""
      if (ndx + 1 == s.length() && s[ndx] == '/') {
	s.erase(ndx, 1);
	continue;
      }

      // Reduce "//" to "/"
      if (ndx + 1 < s.length() && s.substr(ndx,2) == "//") {
	s.erase(ndx, 1);
	continue;
      }

      if (ndx + 2 == s.length() && s.substr(ndx, 2) == "/.") {
	// Strip Trailing "/."
	s.erase(ndx, 2);
	continue;
      }

      if (ndx + 2 < s.length() && s.substr(ndx, 3) == "/./") {
	// Looking at "/./" -- replace with "/" and try again
	s.erase(ndx, 2);
	continue;
      }

      if (ndx + 3 == s.length() && s.substr(ndx, 3) == "/..") {
	// Looking at "/..\0" -- remove this and preceding directory 
	// and start over.
	//
	// There is an open issue here, which is interpretation of
	// "../" after symlink. Regrettably, it is simply not true
	// that "a/b/../c" is always "a/c", but we assume here that
	// it is. That is, we are processing the path LEXICALLY.

	size_t dotDotEnd = ndx+2;

	if (ndx == 0) {
	  /* Path started with /../. Resolution is to strip the
	     leading "/.." and continue. */
	  s.erase(ndx, 3);
	  continue;
	}

	while (ndx > 0 && s[ndx-1] != '/')
	  ndx--;

	// Have either found start of string or a slash. Remove from
	// ndx to dotDotEnd:

	s.erase(ndx, dotDotEnd-ndx);
	continue;
      }

      if (ndx + 3 < s.length() && s.substr(ndx, 3) == "/../") {
	// Looking at "/../" -- remove this and preceding directory 
	// and start over.
	//
	// There is an open issue here, which is interpretation of
	// "../" after symlink. Regrettably, it is simply not true
	// that "a/b/../c" is always "a/c", but we assume here that
	// it is. That is, we are processing the path LEXICALLY.

	size_t dotDotEnd = ndx+3;

	if (ndx == 0) {
	  /* Path started with /../. Resolution is to strip the
	     leading "/.." and continue. */
	  s.erase(ndx, 3);
	  continue;
	}

	while (ndx > 0 && s[ndx-1] != '/')
	  ndx--;

	// Have either found start of string or a slash. Remove from
	// ndx to dotDotEnd:

	s.erase(ndx, dotDotEnd-ndx);
	continue;
      }

      ndx++;
    }

    return Path(s);
  }

  /* This should work for windows too, because in any path where a
   * path_dirname() exists, the colon will be followed by a backslash.
   */
  Path
  Path::directory() const
  {
    if (isEmpty())
      return *this;

    const char *p = c_str();
    const char *last_dirsep = 0;

    for (; *p; p++) {
      if (Path::isDirSep(*p))
	last_dirsep = p;
    }

    if (last_dirsep)
      return Path(s.substr(0, last_dirsep - c_str()));
    else if (isAbsolute())
      return Path::RootDir;
    else
      return Path::CurDir;
  }

  Path::portstat_t
  Path::stat() const
  {
    portstat_t ps;
    struct stat sb;
    int result;
 
    if (isEmpty())
      THROW(excpt::BadValue, "Null or empty filename passed to path_portstat().");

    memset(&ps, 0, sizeof(ps));

    /* Use "lstat" here so we don't traverse any symlinks! Originally we
       used just "stat", and ran into interesting results as OpenCM
       tried to follow the symlinks to their targets. */
    result = lstat(c_str(), &sb);
    if (result == -1) {
      if (errno == ENOENT) {
	ps.exists = false;
	return ps;
      }
      /* A component of the path is not a directory:
         We don't want to just ignore this, because 'update' will fail when it
         tries to write to something inside the "directory". But making the
         error message more descriptive is nice, so users can understand what
         is wrong, and hopefully how to fix it (that may be a bit of a stretch,
         but whatever)
      */
      else if (errno == ENOTDIR) {
	THROW(excpt::NoObject,
	      format("One or more components of %s is not a directory", c_str()));
      }
      else {
	THROW(excpt::NoObject,
	      format("Could not stat path [%s]: errno %d", c_str(), errno));
      }
    }

    ps.exists = true;
    ps.modTime = sb.st_mtime;
    ps.len     = sb.st_size;

    if (S_ISREG(sb.st_mode) && access(c_str(), X_OK) == 0)
      ps.isExec = true;

    /* Decode the file type: */
    ps.type = psUnknown;

    if (S_ISREG(sb.st_mode))
      ps.type = psFile;
    else if (S_ISDIR(sb.st_mode))
      ps.type = psDir;
    else if (S_ISLNK(sb.st_mode))
      ps.type = psSymlink;
    else if (S_ISCHR(sb.st_mode))
      ps.type = psChrDev;

    return ps;
  }

  void
  Path::smkdir() const
  {
    if (isEmpty())
      THROW(excpt::NullArg, "empty path to path_smkdir()");

    Path tmpPath = *this;
    char *start = const_cast<char *>(tmpPath.c_str());
    char *slash = start;

    /* Skip past leading slashes so that the subsequent search for the
       rest of the path doesn't stop with a leading slash:
    */
    while (*slash == '/')
      slash++;
  
    while ((slash = strchr(slash, '/'))) {
      *slash = 0;			/* truncate the path */

      if (!tmpPath.exists())
	tmpPath.mkdir();

      *slash = '/';		/* untruncate the path */
      slash++;
    }

    if (!tmpPath.exists())
      tmpPath.mkdir();
  }

  Path
  Path::resolveLink() const
  {
    if (isEmpty())
      THROW(excpt::NullArg, "Cannot resolveLink() an empty path");

    char buf[PATH_MAX+1];
    size_t len;

    len = readlink(c_str(), buf, PATH_MAX);
    if (len < 1)
      THROW(excpt::NoObject, format("Reading symbolic link %s: errno %d",
			       c_str(), errno));

    buf[len] = 0;

    if (buf[0] == '/')
      return Path(buf);

    return this->directory() + Path(buf);
  }

  bool
  Path::shouldSkipDirent(const std::string& s)
  {
    if (s.empty())
      THROW(excpt::NullArg, "No path to path_should_skip_dirent()");

    if (s == ".")
      return true;

    if (s == "..")
      return true;

    return false;
  }
} /* namespace sherpa */
