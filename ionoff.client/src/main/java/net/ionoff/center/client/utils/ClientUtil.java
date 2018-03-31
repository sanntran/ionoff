package net.ionoff.center.client.utils;

import java.util.HashMap;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.ResponseFormatException;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;

import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.event.UserLogOutEvent;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.shared.dto.MessageDto;

public final class ClientUtil {
	
	private ClientUtil() {
		// prevent installing
	}

	public static boolean isIPAddress(String username) {
		final String d[] = username.split("\\.");
		if (d != null && d.length == 4) {
			for (int i = 0; i < d.length; i++) {
				if (!isIntNumber(d[i])) {
					return false;
				}
			}
			return true;
		}
		else {
			return false;
		}
	}

	public static boolean isIntNumber(String num) {
		try {
			Integer.parseInt(num);
		}
		catch (final NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	public static boolean isLongNumber(String num) {
		try {
			Long.parseLong(num);
		}
		catch (final NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	public static HashMap<String, String> getParamsMap(HandlerManager eventBus, String procedure) {
		final HashMap<String, String> params = new HashMap<String, String>();
		params.put("rpcCommand", procedure);
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return params;
	}

	public static String getUrl(String language, String page) {
		final String url = getBaseUrl() + page + "?locale=" + language;
		return url;
	}

	public static String getBaseUrl() {
		return GWT.getModuleBaseURL().replace((GWT.getModuleName() + "/"), "");
	}

	public static MessageDto handleRpcFailure(Method method, Throwable exception, HandlerManager eventBus) {
		MessageDto messageDto = new MessageDto();
		if (exception instanceof ResponseFormatException) {
			eventBus.fireEvent(new ShowMessageEvent(exception.getMessage(), ShowMessageEvent.ERROR));
			return messageDto;
		}
		final int statusCode = method.getResponse().getStatusCode();
		if (statusCode == Response.SC_UNAUTHORIZED) {
			eventBus.fireEvent(new UserLogOutEvent());
			return messageDto;
		}
		else if (statusCode == Response.SC_FORBIDDEN) {
			eventBus.fireEvent(new ShowMessageEvent(ClientLocale.getClientMessage()
					.accessDenied(), ShowMessageEvent.ERROR));
			return messageDto;
		}
		else if (statusCode == 0 && (method.getResponse().getText() == null || method.getResponse().getText().isEmpty())) {
			eventBus.fireEvent(new ShowMessageEvent(ClientLocale.getClientMessage().serverConnectionError(), ShowMessageEvent.ERROR));
			return messageDto;
		}
		try {
			final JSONObject jsonValue = (JSONObject) JSONParser.parseStrict(method.getResponse().getText());
			try {
				final String status = jsonValue.get("status").toString();
				messageDto.setStatus(Integer.parseInt(status));
			}
			catch (final Exception e) {
				// Ignore number format exception.
			}
			
			try {
				JSONValue jsonCode = jsonValue.get("code");
				if (jsonCode != null) {
					messageDto.setCode(((JSONString)jsonCode).stringValue());
				}
			}
			catch (final Exception e) {
				// Ignore number format exception.
			}
			
			try {
				JSONValue jsonMesage = jsonValue.get("message");
				if (jsonMesage != null) {
					messageDto.setMessage(((JSONString)jsonMesage).stringValue());
				}
			}
			catch (final Exception e) {
				// Ignore number format exception.
			}
			String message = ClientLocale.getClientMessage().serverErrorResponse(statusCode + "", messageDto.getMessage());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
		}
		catch (final Exception e) {
			String message = ClientLocale.getClientMessage().serverErrorResponse(statusCode + "", method.getResponse().getText());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
		}
		return messageDto;
	}
}
