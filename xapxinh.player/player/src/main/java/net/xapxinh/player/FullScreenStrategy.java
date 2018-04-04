package net.xapxinh.player;

import static net.xapxinh.player.Application.application;

import java.awt.Window;

import net.xapxinh.player.event.AfterExitFullScreenEvent;
import net.xapxinh.player.event.BeforeEnterFullScreenEvent;
import uk.co.caprica.vlcj.player.embedded.DefaultAdaptiveRuntimeFullScreenStrategy;

final class FullScreenStrategy extends DefaultAdaptiveRuntimeFullScreenStrategy {

    FullScreenStrategy(Window window) {
        super(window);
    }

    @Override
    protected void beforeEnterFullScreen() {
        application().post(BeforeEnterFullScreenEvent.instance());
    }

    @Override
    protected  void afterExitFullScreen() {
        application().post(AfterExitFullScreenEvent.instance());
    }
}
