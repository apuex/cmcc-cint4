package com.github.apuex.cmcc.cint4.utility;


import org.slf4j.LoggerFactory;

import com.github.apuex.cmcc.cint4.HeartBeatAck;
import com.github.apuex.cmcc.cint4.Message;

import ch.qos.logback.classic.Logger;
import io.netty.channel.ChannelHandlerContext;
import io.netty.util.ReferenceCountUtil;

public class ModifyPasswdHandler extends io.netty.channel.ChannelInboundHandlerAdapter {
	private static final Logger logger = (Logger) LoggerFactory.getLogger(ServerHandler.class);

	@Override
	public void channelActive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] SYN : connection established.", ctx.channel().remoteAddress()));
		ctx.fireChannelActive();
	}

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		logger.info(String.format("[%s] RCV : %s", ctx.channel().remoteAddress(), msg));
		if(msg instanceof Message) {
			Message message = (Message) msg;
			switch(message.PKType) {
			case LOGIN:
				break;
			case LOGIN_ACK:
				break;
			case LOGOUT:
				break;
			case LOGOUT_ACK:
				break;
			case SET_DYN_ACCESS_MODE:
				break;
			case DYN_ACCESS_MODE_ACK:
				break;
			case SET_ALARM_MODE:
				break;
			case ALARM_MODE_ACK:
				break;
			case SEND_ALARM:
				break;
			case SEND_ALARM_ACK:
				break;
			case SYNC_ALARM:
				break;
			case SYNC_ALARM_ACK:
				break;
			case SET_POINT:
				break;
			case SET_POINT_ACK:
				break;
			case MODIFY_PA:
				break;
			case MODIFY_PA_ACK:
				break;
			case HEART_BEAT:
				Message out = new HeartBeatAck(message.SerialNo);
				ctx.writeAndFlush(out);
				logger.info(String.format("[%s] SND : %s", ctx.channel().remoteAddress(), out));
				break;
			case HEART_BEAT_ACK:
				break;
			case TIME_CHECK:
			case TIME_CHECK_ACK:
			default: // Unsupported PKType
				ctx.close();
				break;
			}
		}
		ReferenceCountUtil.release(msg);
	}

	@Override
	public void channelInactive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] RST : connection closed.", ctx.channel().remoteAddress()));
		ctx.fireChannelInactive();
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		logger.info(String.format("[%s] ERR : %s", ctx.channel().remoteAddress(), cause));
		ctx.fireExceptionCaught(cause);
	}
}
