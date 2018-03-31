package net.xapxinh.center.client.player.event;

import org.fusesource.restygwt.client.Method;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class RpcFailureEvent extends GwtEvent<RpcFailureEventHandler> {
	public static Type<RpcFailureEventHandler> TYPE = new Type<RpcFailureEventHandler>();

	private final Method method;
	private final Throwable exception;

	public RpcFailureEvent(Method method, Throwable exception) {
		this.method = method;
		this.exception = exception;
	}

	@Override
	public Type<RpcFailureEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(RpcFailureEventHandler handler) {
		handler.onRpcFailure(this);
	}

	public Method getMethod() {
		return method;
	}

	public Throwable getException() {
		return exception;
	}
}
