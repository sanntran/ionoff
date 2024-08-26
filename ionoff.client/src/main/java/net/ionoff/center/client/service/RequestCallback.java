package net.ionoff.center.client.service;

import com.google.gwt.event.shared.EventBus;
import com.google.gwt.event.shared.HandlerManager;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.utils.ClientUtil;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import static net.ionoff.center.client.utils.ClientUtil.handleRpcFailure;

public class RequestCallback<T> implements MethodCallback<T> {

    @FunctionalInterface
    public interface OnFailure<Method, Throwable> {
        void apply(Method method, Throwable throwable);
    }

    @FunctionalInterface
    public interface OnSuccess<Method, T> {
        void apply(Method method, T body);
    }

    private final HandlerManager eventBus;
    private final OnFailure<Method, Throwable> onFailure;
    private final OnSuccess<Method, T> onSuccess;

    public RequestCallback(HandlerManager eventBus, OnSuccess<Method, T> onSuccess) {
        this.eventBus = eventBus;
        this.onFailure = (method, throwable) -> handleRpcFailure(method, throwable, eventBus);
        this.onSuccess = onSuccess;
    }

    @Override
    public void onFailure(Method method, Throwable throwable) {
        onFailure.apply(method, throwable);
    }

    @Override
    public void onSuccess(Method method, T body) {
        eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
        onSuccess.apply(method, body);
    }
}
