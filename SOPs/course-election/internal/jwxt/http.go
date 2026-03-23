package jwxt

import (
	"errors"
	"net"
	"net/http"
	"time"
)

const (
	retryAttempts       = 3
	retryBackoffBase    = 200 * time.Millisecond
	dialTimeout         = 5 * time.Second
	keepAliveInterval   = 30 * time.Second
	idleConnTimeout     = 90 * time.Second
	tlsHandshakeTimeout = 5 * time.Second
	responseHeaderWait  = 8 * time.Second
)

func newHTTPTransport() *http.Transport {
	return &http.Transport{
		Proxy:                 http.ProxyFromEnvironment,
		DialContext:           (&net.Dialer{Timeout: dialTimeout, KeepAlive: keepAliveInterval}).DialContext,
		ForceAttemptHTTP2:     true,
		MaxIdleConns:          100,
		MaxIdleConnsPerHost:   20,
		MaxConnsPerHost:       50,
		IdleConnTimeout:       idleConnTimeout,
		TLSHandshakeTimeout:   tlsHandshakeTimeout,
		ResponseHeaderTimeout: responseHeaderWait,
		ExpectContinueTimeout: time.Second,
	}
}

func getWithRetry(client *http.Client, endpoint string) (*http.Response, error) {
	var lastErr error

	for attempt := 0; attempt < retryAttempts; attempt++ {
		resp, err := client.Get(endpoint)
		if err == nil && !shouldRetryStatus(resp.StatusCode) {
			return resp, nil
		}
		if err == nil {
			lastErr = errors.New(resp.Status)
			resp.Body.Close()
		} else {
			lastErr = err
		}

		if attempt == retryAttempts-1 {
			break
		}
		time.Sleep(retryBackoffBase * time.Duration(1<<attempt))
	}

	return nil, lastErr
}

func shouldRetryStatus(statusCode int) bool {
	switch statusCode {
	case http.StatusRequestTimeout,
		http.StatusTooManyRequests,
		http.StatusInternalServerError,
		http.StatusBadGateway,
		http.StatusServiceUnavailable,
		http.StatusGatewayTimeout:
		return true
	default:
		return false
	}
}
