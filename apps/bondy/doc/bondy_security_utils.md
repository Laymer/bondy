

# Module bondy_security_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-auth_error_reason">auth_error_reason()</a> ###


<pre><code>
auth_error_reason() = <a href="bondy_oauth2.md#type-error">bondy_oauth2:error()</a> | invalid_scheme | no_such_realm
</code></pre>




### <a name="type-auth_scheme">auth_scheme()</a> ###


<pre><code>
auth_scheme() = wampcra | basic | bearer | digest
</code></pre>




### <a name="type-auth_scheme_val">auth_scheme_val()</a> ###


<pre><code>
auth_scheme_val() = {wampcra, binary(), binary(), map()} | {basic, binary(), binary()} | {bearer, binary()} | {digest, [{binary(), binary()}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-4">authenticate/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="authenticate-4"></a>

### authenticate/4 ###

<pre><code>
authenticate(X1::<a href="#type-auth_scheme">auth_scheme()</a>, Scheme::<a href="#type-auth_scheme_val">auth_scheme_val()</a>, Realm::<a href="#type-uri">uri()</a>, Peer::<a href="bondy_session.md#type-peer">bondy_session:peer()</a>) -&gt; {ok, <a href="bondy_security.md#type-context">bondy_security:context()</a> | map()} | {error, <a href="#type-auth_error_reason">auth_error_reason()</a>}
</code></pre>
<br />

