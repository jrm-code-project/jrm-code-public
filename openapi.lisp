;;; -*- Lisp -*-

;;; Public OpenAPI 3.0 discovery document for the programmatic API (see
;;; API-TOKEN-ROUTES.LISP's POST /api/v1/auth/token, the first endpoint
;;; described here). Served as a plain static YAML string at
;;; GET /openapi.yaml -- intentionally NOT wrapped in
;;; WITH-API-AUTH-AND-RATE-LIMIT (see API-MIDDLEWARE.LISP): this document
;;; must be discoverable by unauthenticated tooling (e.g. an OpenAPI
;;; codegen client or API explorer) before it has ever obtained a JWT.

(in-package "JRM-CODE-PROJECT")

(defparameter *openapi-spec*
  "openapi: 3.0.3
info:
  title: JRM Code Project API
  version: 1.0.0
servers:
  - url: https://jrm-code-project.com
paths:
  /api/v1/auth/token:
    post:
      summary: Exchange an API key for a short-lived JWT.
      description: Exchange an API key for a short-lived JWT.
      requestBody:
        required: true
        content:
          application/json:
            schema:
              type: object
              required:
                - username
                - api_key
              properties:
                username:
                  type: string
                api_key:
                  type: string
      responses:
        '200':
          description: Success.
          content:
            application/json:
              schema:
                type: object
                properties:
                  access_token:
                    type: string
                  expires_in:
                    type: integer
                  token_type:
                    type: string
        '400':
          description: Bad Request.
        '401':
          description: Unauthorized (Invalid credentials).
        '429':
          description: Too Many Requests.
  /api/v1/pastes:
    post:
      summary: Create a new paste.
      description: Create a new paste owned by the authenticated caller.
      security:
        - BearerAuth: []
      requestBody:
        required: true
        content:
          application/json:
            schema:
              type: object
              required:
                - content
              properties:
                content:
                  type: string
      responses:
        '201':
          description: Created.
          content:
            application/json:
              schema:
                type: object
                properties:
                  status:
                    type: string
                  id:
                    type: string
        '400':
          description: Bad Request.
        '401':
          description: Unauthorized or expired token.
        '429':
          description: Too Many Requests.
    get:
      summary: Retrieve a paste.
      description: Retrieve a paste's content by id. Publicly readable; no authentication required.
      security: []
      parameters:
        - name: id
          in: query
          required: true
          schema:
            type: string
      responses:
        '200':
          description: Success.
          content:
            application/json:
              schema:
                type: object
                properties:
                  id:
                    type: string
                  content:
                    type: string
        '400':
          description: Bad Request.
        '404':
          description: Paste not found or expired.
    delete:
      summary: Delete a paste.
      description: Delete a paste owned by the authenticated caller.
      security:
        - BearerAuth: []
      parameters:
        - name: id
          in: query
          required: true
          schema:
            type: string
      responses:
        '200':
          description: Success.
          content:
            application/json:
              schema:
                type: object
                properties:
                  status:
                    type: string
        '400':
          description: Bad Request.
        '401':
          description: Unauthorized or expired token.
        '429':
          description: Too Many Requests.
  /api/v1/user/pastes:
    get:
      summary: List the authenticated caller's pastes.
      description: Return the authenticated caller's non-expired pastes, most recently created first.
      security:
        - BearerAuth: []
      responses:
        '200':
          description: Success.
          content:
            application/json:
              schema:
                type: array
                items:
                  type: object
                  properties:
                    id:
                      type: string
                    created_at:
                      type: string
                    expires_at:
                      type: string
                      nullable: true
                    content_preview:
                      type: string
        '401':
          description: Unauthorized or expired token.
        '429':
          description: Too Many Requests.
components:
  securitySchemes:
    BearerAuth:
      type: http
      scheme: bearer
      bearerFormat: JWT
security:
  - BearerAuth: []
  - {}
"
  "Raw OpenAPI 3.0 YAML text describing this application's programmatic
API (POST /api/v1/auth/token, POST/GET/DELETE /api/v1/pastes, and GET
/api/v1/user/pastes), served verbatim (no parsing/templating) at GET
/openapi.yaml by OPENAPI-SPEC-ACTION. Kept as a single embedded string
literal -- this is a small, hand-maintained document, so pulling in a
YAML-generation library would be more machinery than the problem needs.
The document-level `security: - BearerAuth: [] / - {}' marks BearerAuth
as available but optional by default, and most endpoints narrow this
with a per-operation `security' override -- required BearerAuth for
anything acting on behalf of a specific user, none for the publicly-
readable GET /api/v1/pastes and this document itself.")

(defparameter *openapi-spec-json*
  "{
  \"openapi\": \"3.0.3\",
  \"info\": {
    \"title\": \"JRM Code Project API\",
    \"version\": \"1.0.0\"
  },
  \"servers\": [
    { \"url\": \"https://jrm-code-project.com\" }
  ],
  \"paths\": {
    \"/api/v1/auth/token\": {
      \"post\": {
        \"summary\": \"Exchange an API key for a short-lived JWT.\",
        \"description\": \"Exchange an API key for a short-lived JWT.\",
        \"requestBody\": {
          \"required\": true,
          \"content\": {
            \"application/json\": {
              \"schema\": {
                \"type\": \"object\",
                \"required\": [\"username\", \"api_key\"],
                \"properties\": {
                  \"username\": { \"type\": \"string\" },
                  \"api_key\": { \"type\": \"string\" }
                }
              }
            }
          }
        },
        \"responses\": {
          \"200\": {
            \"description\": \"Success.\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"access_token\": { \"type\": \"string\" },
                    \"expires_in\": { \"type\": \"integer\" },
                    \"token_type\": { \"type\": \"string\" }
                  }
                }
              }
            }
          },
          \"400\": { \"description\": \"Bad Request.\" },
          \"401\": { \"description\": \"Unauthorized (Invalid credentials).\" },
          \"429\": { \"description\": \"Too Many Requests.\" }
        }
      }
    },
    \"/api/v1/pastes\": {
      \"post\": {
        \"summary\": \"Create a new paste.\",
        \"description\": \"Create a new paste owned by the authenticated caller.\",
        \"security\": [ { \"BearerAuth\": [] } ],
        \"requestBody\": {
          \"required\": true,
          \"content\": {
            \"application/json\": {
              \"schema\": {
                \"type\": \"object\",
                \"required\": [\"content\"],
                \"properties\": {
                  \"content\": { \"type\": \"string\" }
                }
              }
            }
          }
        },
        \"responses\": {
          \"201\": {
            \"description\": \"Created.\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"status\": { \"type\": \"string\" },
                    \"id\": { \"type\": \"string\" }
                  }
                }
              }
            }
          },
          \"400\": { \"description\": \"Bad Request.\" },
          \"401\": { \"description\": \"Unauthorized or expired token.\" },
          \"429\": { \"description\": \"Too Many Requests.\" }
        }
      },
      \"get\": {
        \"summary\": \"Retrieve a paste.\",
        \"description\": \"Retrieve a paste's content by id. Publicly readable; no authentication required.\",
        \"security\": [],
        \"parameters\": [
          {
            \"name\": \"id\",
            \"in\": \"query\",
            \"required\": true,
            \"schema\": { \"type\": \"string\" }
          }
        ],
        \"responses\": {
          \"200\": {
            \"description\": \"Success.\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"id\": { \"type\": \"string\" },
                    \"content\": { \"type\": \"string\" }
                  }
                }
              }
            }
          },
          \"400\": { \"description\": \"Bad Request.\" },
          \"404\": { \"description\": \"Paste not found or expired.\" }
        }
      },
      \"delete\": {
        \"summary\": \"Delete a paste.\",
        \"description\": \"Delete a paste owned by the authenticated caller.\",
        \"security\": [ { \"BearerAuth\": [] } ],
        \"parameters\": [
          {
            \"name\": \"id\",
            \"in\": \"query\",
            \"required\": true,
            \"schema\": { \"type\": \"string\" }
          }
        ],
        \"responses\": {
          \"200\": {
            \"description\": \"Success.\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"object\",
                  \"properties\": {
                    \"status\": { \"type\": \"string\" }
                  }
                }
              }
            }
          },
          \"400\": { \"description\": \"Bad Request.\" },
          \"401\": { \"description\": \"Unauthorized or expired token.\" },
          \"429\": { \"description\": \"Too Many Requests.\" }
        }
      }
    },
    \"/api/v1/user/pastes\": {
      \"get\": {
        \"summary\": \"List the authenticated caller's pastes.\",
        \"description\": \"Return the authenticated caller's non-expired pastes, most recently created first.\",
        \"security\": [ { \"BearerAuth\": [] } ],
        \"responses\": {
          \"200\": {
            \"description\": \"Success.\",
            \"content\": {
              \"application/json\": {
                \"schema\": {
                  \"type\": \"array\",
                  \"items\": {
                    \"type\": \"object\",
                    \"properties\": {
                      \"id\": { \"type\": \"string\" },
                      \"created_at\": { \"type\": \"string\" },
                      \"expires_at\": { \"type\": \"string\", \"nullable\": true },
                      \"content_preview\": { \"type\": \"string\" }
                    }
                  }
                }
              }
            }
          },
          \"401\": { \"description\": \"Unauthorized or expired token.\" },
          \"429\": { \"description\": \"Too Many Requests.\" }
        }
      }
    }
  },
  \"components\": {
    \"securitySchemes\": {
      \"BearerAuth\": {
        \"type\": \"http\",
        \"scheme\": \"bearer\",
        \"bearerFormat\": \"JWT\"
      }
    }
  },
  \"security\": [
    { \"BearerAuth\": [] },
    {}
  ]
}
"
  "Raw OpenAPI 3.0 JSON text, semantically identical to *OPENAPI-SPEC*
(the YAML document), served verbatim at GET /openapi.json by
OPENAPI-SPEC-JSON-ACTION for tooling that prefers/requires JSON over
YAML. Kept as a second hand-maintained string literal alongside
*OPENAPI-SPEC* rather than derived from it at runtime, matching this
file's existing YAML-string convention -- both documents are small
enough to keep in sync by hand, and doing so avoids pulling in a YAML
parser purely to round-trip this document into JSON.")

(hunchentoot:define-easy-handler (openapi-spec-action :uri "/openapi.yaml") ()
  ;; Deliberately not wrapped in WITH-API-AUTH-AND-RATE-LIMIT (see
  ;; API-MIDDLEWARE.LISP) or gated on an authenticated session: this
  ;; document must be fetchable by machines (codegen tools, API
  ;; explorers) with no prior credentials at all.
  (setf (hunchentoot:content-type*) "application/yaml")
  (case (hunchentoot:request-method*)
    (:get *openapi-spec*)
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     "")))

(hunchentoot:define-easy-handler (openapi-spec-json-action :uri "/openapi.json") ()
  ;; Same discoverability rationale as OPENAPI-SPEC-ACTION above: no
  ;; auth/rate-limit gate, since tooling needs this before it has any
  ;; credentials.
  (setf (hunchentoot:content-type*) "application/json")
  (case (hunchentoot:request-method*)
    (:get *openapi-spec-json*)
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     "")))
