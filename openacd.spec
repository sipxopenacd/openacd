Name: openacd
Version: 2.0.0
Summary: OpenACD Call Center
Release: %{?buildno:%buildno}%{!?buildno:1}
Group: Applications/Communications
Vendor: OpenACD
Packager: Douglas Hubler
License: CPAL
AutoReqProv: no
URL: http://github.com/OpenACD/OpenACD/wiki
Source: %{name}-%{version}.tar.gz

# AFAIK: >= 15, but need to force centos 6 to ignore 14B that comes w/centos
BuildRequires: erlang = R15B
BuildRequires: rebar
BuildRequires: erlang-ej
BuildRequires: erlang-ejrpc2
BuildRequires: erlang-gen_leader
BuildRequires: erlang-gen_server_mock
BuildRequires: erlang-lager
BuildRequires: erlang-cowboy
BuildRequires: erlang-mimetypes
BuildRequires: erlang-gproc
BuildRequires: erlang-meck
BuildRequires: erlmongo
BuildRequires: openssh

Requires: erlang = R15B
Requires: erlang-ej
Requires: erlang-ejrpc2
Requires: erlang-gen_leader
Requires: erlang-gen_server_mock
Requires: erlang-lager
Requires: erlang-cowboy
Requires: erlang-mimetypes
Requires: erlang-gproc
Requires: erlang-meck
Requires: erlmongo

BuildRoot: %{_builddir}/%{name}-root

%description
OpenACD is a skills-based, Call Center software based on FreeSWITCH and built in erlang.

%package devel
Group: Applications/Communications
Vendor: OpenACD
Summary: Header files for %name
Requires: %name >= %version

%description devel
Erlang Header files for %name

%prep
%setup -n %{name}-%{version} 

%build
%configure runuser=openacd
make

%install
make install DESTDIR=$RPM_BUILD_ROOT PREFIX=%{prefix}

%clean
rm -rf $RPM_BUILD_ROOT

%pre
if ! /usr/bin/id -g openacd >/dev/null 2>&1; then
  /usr/sbin/groupadd -r openacd
fi

if ! /usr/bin/id openacd >/dev/null 2>&1; then
  /usr/sbin/useradd -M -r -g openacd -d %{_sysconfdir}/openacd -s /bin/false -c openacd openacd 2>&1
fi

%post

%files
%defattr(-,root,root,-)
%dir %attr(755,openacd,openacd) %{_localstatedir}/log/openacd
%dir %attr(755,openacd,openacd) %{_localstatedir}/openacd/db
%dir %attr(755,openacd,openacd) %{_localstatedir}/openacd/run
%dir %attr(600,openacd,openacd) %{_localstatedir}/openacd/key
%attr(400,openacd,openacd) %{_localstatedir}/openacd/key/openacd.key*
%{_libdir}/erlang/lib/openacd-%{version}/ebin/*
%attr(755,root,root) %{_bindir}/openacd
%{_sysconfdir}/openacd/sys.config
%{_sysconfdir}/openacd/env.config

%files devel
%{_libdir}/erlang/lib/openacd-%{version}/include/*

%changelog
* Thu Jan 28 2013 Douglas Hubler <dhubler@ezuce.com>
- Update for 2.0.0
* Thu Apr 25 2012 Cristi Starasciuc <cristi@ezuce.com>
- New packaging
* Fri Jun 24 2011 Micah Warren <micahw@lordnull.com>
- Updated provider, url, and removed no longer needed enviroment variables.
* Thu Jan 27 2011 Douglas Hubler <douglas@hubler.us>
- Initial release
