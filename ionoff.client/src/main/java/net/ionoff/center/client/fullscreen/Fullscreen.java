package net.ionoff.center.client.fullscreen;

/**
 * Fullscreen commands<br>
 * <a href="http://hacks.mozilla.org/2012/01/using-the-fullscreen-api-in-web-browsers/">Mozilla Hacks</a> <br>
 * <a href="https://github.com/robnyman/robnyman.github.com/blob/master/fullscreen/js/base.js">Javascript Example</a> <br>
 * <a hef="http://msdn.microsoft.com/en-us/library/ie/dn265028%28v=vs.85%29.aspx">MSDN</a>
 * 
 * @author wokier
 */
public class Fullscreen {

	private Fullscreen() {
	}

	public static native void requestFullscreen()
	/*-{
		var docElement = $doc.documentElement;
		if (docElement.requestFullscreen) {
			docElement.requestFullscreen();
		} else if (docElement.msRequestFullscreen) {
			docElement.msRequestFullscreen();
		} else if (docElement.mozRequestFullScreen) {
			docElement.mozRequestFullScreen();
		} else if (docElement.webkitRequestFullScreen) {
			docElement.webkitRequestFullScreen();
		}
	}-*/;

	public static void requestFullscreen(boolean input) {
		if (input) {
			requestFullscreenWithInput();
		}
		requestFullscreen();
	}

	public static native void requestFullscreenWithInput()
	/*-{
		var docElement = $doc.documentElement;
		if (docElement.webkitRequestFullScreen) {
			docElement.webkitRequestFullScreen(Element.ALLOW_KEYBOARD_INPUT);
		} else if (docElement.requestFullscreen) {
			docElement.requestFullscreen();
		} else if (docElement.msRequestFullscreen) {
			docElement.msRequestFullscreen();
		} else if (docElement.mozRequestFullScreen) {
			docElement.mozRequestFullScreen();
		}
	}-*/;

	public static native void exitFullscreen()
	/*-{
		if ($doc.exitFullscreen) {
			$doc.exitFullscreen();
		} else if ($doc.msExitFullscreen) {
			$doc.msExitFullscreen();
		} else if ($doc.mozCancelFullScreen) {
			$doc.mozCancelFullScreen();
		} else if ($doc.webkitCancelFullScreen) {
			$doc.webkitCancelFullScreen();
		}
	}-*/;
	
	public static native boolean isFullScreen() /*-{
		//return $doc.fullscreenEnabled || $doc.mozFullScreenEnabled || $doc.webkitFullscreenEnabled;
		//document.fullscreenElement || document.mozFullScreenElement || document.webkitFullscreenElement;
		if($doc.fullscreenEnabled) {
			return ($doc.fullscreenElement !== null);
		} else if($doc.mozFullScreenEnabled) {
			return ($doc.mozFullScreenElement !== null);;
		} else if($doc.webkitFullscreenEnabled) {
			return ($doc.webkitFullscreenElement !== null);;
		}
		return false;
	}-*/;
}