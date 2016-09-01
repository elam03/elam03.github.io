
URL: Uniform Resource Locator
* Query Parameters (GET parameters) http://example.com/foo?p=foo&z=p -> z = p; p = foo;
* Fragment http://www.example.com/foo?a=foo&b=bar#fragment -> fragment = fragment
* Port http://localhost:8080/ (default = 80)

HTTP: HyperText Transfer Protocol
* method path version: example: GET /foo HTTP/1.1 - request line
GET  - request data from server
POST - send data to server

***HTTP Header Fields

***HTTP Responses
Request Line -> Status line
Get /foo HTTP/1.1 -> HTTP/1.1 200 OK

**Status Codes
* 200 OK
* 302 Found
* 404 Not Found
* 500 Server Error

*** HTTP Response Headers
HTTP/1.1 200 OK
Date: Tue Mar 2012 04:33:33 GMT
Server: Apache/2.2.3 <- Try not to include for security reasons!
Content-Type: text/html;
Content-Length: 1539
