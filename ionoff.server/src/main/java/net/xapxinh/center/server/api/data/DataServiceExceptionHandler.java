package net.xapxinh.center.server.api.data;

import java.io.IOException;

import net.xapxinh.center.server.exception.DataServiceException;

import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.DefaultResponseErrorHandler;
import org.springframework.web.client.ResponseErrorHandler;

import com.google.common.base.Charsets;
import com.google.common.io.ByteStreams;
import com.google.gson.Gson;

public class DataServiceExceptionHandler implements ResponseErrorHandler {

	private final Gson gson = new Gson();
    private final ResponseErrorHandler errorHandler = new DefaultResponseErrorHandler();

    @Override
	public boolean hasError(ClientHttpResponse response) throws IOException {
        return errorHandler.hasError(response);
    }

    @Override
	public void handleError(ClientHttpResponse response) throws IOException {
    	final int statusCode = response.getStatusCode().value();
    	if (statusCode == 0) {
    		throw new DataServiceConnectException();
    	}
    	final String resp = new String(ByteStreams.toByteArray(response.getBody()), Charsets.UTF_8);
    	final DataServiceMesage message = gson.fromJson(resp, DataServiceMesage.class);
    	if (UnknownPlayerException.class.getSimpleName().equals(message.getClazz())) {
    		throw new UnknownPlayerException(message.getMessage());
    	}
    	throw new DataServiceException(message.getMessage());
    }
}