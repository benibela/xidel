#
# spec file for package xidel
#
# Copyright (c) 2012 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.
#
# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

Name:           xidel
Version:        0.9.6
Release:        1
Summary:        A tool for querying local and remote XML/HTML/JSON data

License:        GPLv3+
Group:          Productivity/Networking/Web/Utilities
URL:            http://www.videlibri.de/xidel.html
Source0:        %{name}-%{version}.src.tar.gz 

BuildRequires:  fpc
#%if 0%{?suse_version}
#%endif
#%if 0%{?fedora} || 0%{?centos_version} || 0%{?mandriva_version}
#%endif
#%if 0%{?rhel_version}
#%endif

%description
Xidel is a command line tool to query data from HTML/XML web pages, JSON-APIs and local files.

It implements interpreters for XPath 2, XPath 3, XQuery 1, XQuery 3, JSONiq, CSS selectors and custom pattern matching.

XPath and CSS selectors are the most efficient way to select certain elements from XML/HTML documents.
 JSONiq (with custom extensions) is an easy way to select data from JSON.

XQuery is a Turing-complete superset of XPath and allows arbitrary data transformations and the creation of new documents.

Pattern matching is for XML/HTML documents what regular expressions are for plaintext, i.e. pattern matching behaves like a regular expression over the space of tags, instead over the space of characters.

Xidel implements a kind of internal pipes to pipe HTTP requests from one query to the next, so there is no need to distinguish selecting links and downloading the data referenced by them. Therefore arbitrary complex queries going over arbitrary many pages can be executed with a single call of Xidel.
 
%prep
%setup -n %{name}-%{version}-src

%build
programs/internet/xidel/build.sh

%install
programs/internet/xidel/install.sh %{buildroot}
 
%files
%defattr(-,root,root,-)
#%dir %{_datadir}/icons/hicolor
#%{_datadir}/texstudio/
%{_bindir}/xidel
 
%changelog

