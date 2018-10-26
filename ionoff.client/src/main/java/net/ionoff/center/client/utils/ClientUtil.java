package net.ionoff.center.client.utils;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.Window;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.event.UserLogOutEvent;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.shared.cookie.Service;
import net.ionoff.center.shared.dto.MessageDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.ResponseFormatException;

import java.util.HashMap;

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
		final String url = getClientUrl() + page + "?locale=" + language;
		return url;
	}

	public static String getBaseUrl() {
		String protocol = Window.Location.getProtocol();
    	if (protocol.contains("http")) {
    		String port = Window.Location.getPort();
    		if (!"8008".equals(port) && !"80".equals(port)) { // production port
    			port = "8208"; // development mode
		    }
    		return protocol + "//" + Window.Location.getHostName() + ":" + port;
    	}
    	Service server = StorageService.getInstance().getEnabledApiServer();
		if (server == null) {
			return "http://cloud.ionoff.net"; 	
		}
		if (server.getHost().startsWith("http")) {
			return server.getHost();  
		}
		return "http://" + server.getHost();  
	}
	
	public static String getClientUrl() {
		return GWT.getModuleBaseURL().replace((GWT.getModuleName() + "/"), "");
	}
	
	public static String getServiceUrl() {
		return getBaseUrl() + "/center";
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

	public static String format(final String format, String delimiter, final String... args) {
		String[] split = format.split(delimiter);
		final StringBuffer buffer = new StringBuffer();
		if (format.startsWith(delimiter)) {
			buffer.append(args[0]);
		}
		for (int i= 0; i < split.length - 1; i+= 1) {
			buffer.append(split[i]);
			buffer.append(args[i]);
		}
		buffer.append(split[split.length - 1]);
		if (format.endsWith(delimiter)) {
			buffer.append(args[split.length - 1]);
		}
		return buffer.toString();
	}
}
