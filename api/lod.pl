/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Michiel Hildebrand
    Author:        Jan Wielemaker
    E-mail:        michielh@few.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(api_lod,
	  [ lod_api/1			% +Request
	  ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_json)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_describe)).
:- use_module(library(settings)).
:- use_module(library(rdf_write)).
:- use_module(library(uri)).

:- use_module(applications(browse)).


/** <module> LOD - Linked Open Data server

Linked (Open) Data turns RDF URIs   (indentifiers) into URLs (locators).
Requesting the data  behind  the  URL   returns  a  description  of  the
resource. So, if we see   a resource http://example.com/employe/bill, we
get do an HTTP GET  request  and   expect  to  receive  a description of
_bill_.  This module adds LOD facilities to ClioPatria.

---++ Running the LOD server

There are several ways to run the LOD server.

    1. The simplest way to realise LOD is to run ClioPatria there where
    the authority component of the URL points to (see uri_components/2
    for decomposing URIs).  This implies you must be able to create a
    DNS binding for the host and be able to run ClioPatria there.

    2. Sometimes the above does not work, because the port is already
    assigned to another machine, you are not allowed to run ClioPatria
    on the target host, the target is behind a firewall, etc. In that
    case, notable if the host runs Apache, you can exploit the Apache
    module =mod_proxy= and proxy the connections to a location where
    ClioPatria runs. If you ensure that the path on Apache is the same
    as the path on ClioPatria, the following Apache configuration rule
    solves the problem:

    ==
    ProxyPass /rdf/ http://cliopatria-host:3020/rdf/
    ==

    3. Both above methods require no further configuration.
    Unfortunately, they require a registered domain control over DNS
    and administrative rights over certain machines.  A solution that
    doesn't require this is to use www.purl.org.  This allows you to
    redirect URLs within the purl domain to any location you control.
    The redirection method can be defined with purl.  In the semantic
    web community, we typically use *|See other|* (303).  The catch
    is that if the address arrives at ClioPatria, we no longer know
    where it came from.  This is not a problem in (1), as there was
    no redirect.  It is also not a problem in (2), because Apache
    adds a header =|x-forwarded-host|=.  Unfortunately, there is
    no way to tell you are activated through a redirect, let alone
    where the redirect came from.

    To deal with this situation, we define an additional option for
    http_handler/3, redirected_from. For example, if
    http://www.purl.org/vocabularies/myvoc/ is redirected to /myvoc/
    on ClioPatria, we use:

    ==
    :- http_handler('/myvoc/', lod_api,
		    [ redirected_from('http://www.purl.org/vocabularies/myvoc/'),
		      prefix
		    ]).
    ==

By default, there is no HTTP handler  pointing to lod_api/1. The example
above describes how to deal with redirected  URIs. The cases (1) and (2)
must also be implemented by registering a  handler. This can be as blunt
as registering a handler for the root   of the server, but typically one
would use one or more handlers  that   deal  with  sub-trees that act as
Linked Data repositories.  Handler  declarations   should  use  absolute
addresses to guarantee a match with the RDF  URIs, even if the server is
relocated by means of the http:prefix setting.  For example:

    ==
    :- http_handler('/rdf/', lod_api, [prefix]).
    ==

@see http://linkeddata.org/
*/

:- setting(lod:redirect, boolean, false,
	   'If true, redirect from accept-header to extension').

%%	lod_api(+Request)
%
%	Reply to a Linked Data request. The  handler is capable of three
%	output formats. It decides on the   desired  format based on the
%	HTTP =Accept= header-field. If no acceptable format is found, it
%	replies with a human-readable description  of the resource using
%	ClioPatria RDF browser-page as defined by list_resource//2.


lod_api(Request) :-
	lod_uri(Request, URI),
	(   memberchk(accept(AcceptHeader), Request)
	->  http_parse_header_value(accept, AcceptHeader, AcceptList)
	;   AcceptList = []
	),
	lod_request(URI, AcceptList, Request).

lod_request(URI, AcceptList, Request) :-
	rdf_subject(URI), !,
	preferred_format(AcceptList, Format),
	(   cliopatria:redirect_uri(Format, URI, SeeOther)
	->  http_redirect(see_other, SeeOther, Request)
	;   setting(lod:redirect, true),
	    redirect(URI, AcceptList, SeeOther)
	->  http_redirect(see_other, SeeOther, Request)
	;   lod_describe(Format, URI)
	).
lod_request(URL, _AcceptList, _Request) :-
	format_request(URL, URI, Format), !,
	lod_describe(Format, URI).
lod_request(URI, _AcceptList, _Request) :-
	throw(http_reply(not_found(URI))).


%%	lod_uri(+Request, -URI)
%
%	URI is the originally requested URI.   This predicate deals with
%	redirections if the HTTP handler was registered using the option
%	redirected_from(URL). Otherwise it resolves   the correct global
%	URI using http_current_host/4.

lod_uri(Request, URI) :-
	handler_options(Request, Location, Options),
	memberchk(redirected_from(Org), Options),
	memberchk(request_uri(ReqURI), Request),
	atom_concat(Location, Rest, ReqURI),
	atom_concat(Org, Rest, URI).
lod_uri(Request, URI) :-
	memberchk(request_uri(ReqURI), Request),
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	(   Port == 80
	->  atomic_list_concat(['http://', Host, ReqURI], URI)
	;   atomic_list_concat(['http://', Host, :, Port, ReqURI], URI)
	).


%%	handler_options(+Request, -Location, -Options) is det.
%
%	True if Options are  the  options   that  are  defined  with the
%	HTTP-handler that processes request.

handler_options(Request, Location, Options) :-
	memberchk(path(Path), Request),
	(   memberchk(path_info(Rest), Request),
	    atom_concat(Location, Rest, Path)
	->  true
	;   Location = Path
	),
	http_current_handler(Location, _:_, Options).


%%	redirect(+URI, +AcceptList, -RedirectURL)
%
%	Succeeds if URI is in the store and a RedirectURL is found for
%	it.

redirect(URI, AcceptList, To) :-
	rdf_subject(URI),
	preferred_format(AcceptList, Format),
	(   cliopatria:redirect_url(Format, URI, To)
	->  true
	;   uri_components(URI, URIComponents),
	    uri_data(path, URIComponents, Path0),
	    format_suffix(Format, Suffix),
	    file_name_extension(Path0, Suffix, Path),
	    uri_data(path, URIComponents, Path, ToComponents),
	    uri_components(To, ToComponents)
	).


%%	preferred_format(+AcceptList, -Format) is det.
%
%	Format is the highest ranked mimetype found in the Acceptlist of
%	the request and that  we  can   support.  Expects  an AcceptList
%	sorted by rank.

preferred_format(AcceptList, Format) :-
	member(media(MimeType,_,_,_), AcceptList),
	ground(MimeType),
	mimetype_format(MimeType, Format), !.
preferred_format(_, html).


%%	format_request(+URL, -URI, -Format) is semidet.
%
%	True if URL contains a suffix   that  corresponds to a supported
%	output format, and the global URI occurs in the database.

format_request(URL, URI, Format) :-
	uri_components(URL, URLComponents),
	uri_data(path, URLComponents, Path),
	file_name_extension(Base, Ext, Path),
	(   format_suffix(Format, Ext),
	    mimetype_format(_, Format)
	->  true
	),
	uri_data(path, URLComponents, Base, PlainComponents),
	uri_components(URI, PlainComponents),
	rdf_subject(URI).


%%	lod_describe(+Format, +URI) is det.
%
%	Write an HTTP document  describing  URI   to  in  Format  to the
%	current output. Format is defined by mimetype_format/2.

lod_describe(html, URI) :- !,
	rdf_display_label(URI, Label),
	reply_html_page(cliopatria(default),
			title('Resource ~w'-[Label]),
			\list_resource(URI, [])).
lod_describe(Format, URI) :-
	lod_description(URI, RDF),
	send_graph(Format, RDF).

send_graph(xmlrdf, RDF) :-
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n'),
	rdf_write_xml(current_output, RDF).
send_graph(json, RDF) :-
	graph_json(RDF, JSON),
	reply_json(JSON).


%%	lod_description(+URI, -RDF) is det.
%
%	RDF is a  graph  represented  as   a  list  of  rdf(S,P,O)  that
%	describes URI.
%
%	This predicate is hooked   by  cliopatria:lod_description/2. The
%	default is implemented by resource_CBD/3.
%
%	@see SPARQL DESCRIBE

lod_description(URI, RDF) :-
	cliopatria:lod_description(URI, RDF), !.
lod_description(URI, RDF) :-
	resource_CBD(rdf, URI, RDF).


%%	mimetype_format(?MimeType, ?Format) is nondet.
%
%	Conversion between mimetypes and formats.

mimetype_format(application/'rdf+xml', xmlrdf).
mimetype_format(application/json,      json).
mimetype_format(text/html,	       html).

%%	format_suffix(?Format, ?Suffix) is nondet.
%
%	Suffix is the file name extension used for Format.

format_suffix(xmlrdf, rdf).
format_suffix(json,   json).
format_suffix(html,   html).


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	cliopatria:redirect_uri/3,
	cliopatria:lod_description/2.

%%	cliopatria:redirect_uri(+Format, +URI, -RedirectURL)
%
%	Compose a RedirectionURL based on the  output Format and the URI
%	that is in our RDF database. For example, this could map the URI
%	http://example.com/employe/bill   into   Bill's    homepage   at
%	http://example.com/~bill if Format is =html=.  The default is to
%	a format-specific extension  to  the   path  component  of  URI,
%	returning  e.g.,  http://example.com/employe/bill.rdf    if  the
%	requested format is RDF.
%
%       @see This hook is used by redirect/3.
%       @param Format is one of =xmlrdf=, =json= or =html=.


%%	cliopatria:lod_description(+URI, -RDF:list(rdf(s,p,o)))
%
%	RDF is list of triples describing URI. The default is to use the
%	Concise Bounded Description as implemented by graph_CBD/3.
%
%	@see This hook is used by lod_description/2






