package net.ionoff.center.client.mediaplayer.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface RpcFailureEventHandler extends EventHandler {
	void onRpcFailure(RpcFailureEvent rpcFailureEvent);
}